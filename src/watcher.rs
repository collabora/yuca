use std::{
    collections::HashMap,
    hash::Hash,
    io::IoSliceMut,
    ops::ControlFlow,
    pin::Pin,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex, Weak,
    },
};

use camino::Utf8Path;
use futures_core::Stream;
use nix::{
    cmsg_space,
    sys::socket::{
        bind, recvmsg, shutdown, socket, AddressFamily, MsgFlags, NetlinkAddr, Shutdown, SockFlag,
        SockProtocol, SockType, UnixCredentials,
    },
};
use rustix::fd::{AsFd, AsRawFd, BorrowedFd, OwnedFd, RawFd};
use strum::EnumString;

use crate::{sysfs::*, Error, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString)]
#[strum(serialize_all = "lowercase")]
enum Action {
    // No Add or Remove, because we don't really care about those events here,
    // so failing to parse is fine.
    Bind,
    Change,
    Unbind,
}

#[derive(Debug, Clone)]
struct Uevent<'a> {
    action: Action,
    devpath: &'a str,
    devtype: Option<&'a str>,
    subsystem: &'a str,
}

impl<'a> Uevent<'a> {
    fn parse(s: &'a str) -> Result<Self> {
        let mut action = None;
        let mut devpath = None;
        let mut devtype = None;
        let mut subsystem = None;

        for line in s
            .split_terminator('\0')
            // First line is just "action@devpath", so ignore it.
            .skip(1)
        {
            let (k, v) = line.split_once('=').ok_or(Error::Parse)?;
            match k {
                "ACTION" => action = Some(Action::from_str(v)?),
                "DEVPATH" => devpath = Some(v),
                "DEVTYPE" => devtype = Some(v),
                "SUBSYSTEM" => subsystem = Some(v),
                _ => (),
            }
        }

        let (Some(action), Some(devpath), Some(subsystem)) = (action, devpath, subsystem) else {
            return Err(Error::Parse);
        };

        Ok(Uevent {
            action,
            devpath,
            devtype,
            subsystem,
        })
    }
}

#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
#[error("channel overflowed, missed {missed_events} events")]
pub struct Overflowed {
    pub missed_events: u64,
}

#[derive(Debug)]
pub struct EventStream<T: Clone>(async_broadcast::Receiver<T>);

impl<T: Clone> Stream for EventStream<T> {
    type Item = std::result::Result<T, Overflowed>;

    fn poll_next(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        Pin::new(&mut self.0)
            .poll_recv(cx)
            .map_err(|err| match err {
                async_broadcast::RecvError::Overflowed(missed_events) => {
                    Overflowed { missed_events }
                }
                // Should not happen, because poll_recv would return None
                // instead.
                async_broadcast::RecvError::Closed => unreachable!(),
            })
    }
}

struct Channel<T> {
    send: async_broadcast::Sender<T>,
    // An inactive receiver is saved vs a regular/active one because it lets us
    // see when there are 0 active receivers left and then destroy both channel
    // ends.
    recv: async_broadcast::InactiveReceiver<T>,
}

pub(crate) struct ChannelMap<P: Eq + Hash, V: Clone>(Mutex<HashMap<P, Channel<V>>>);

impl<P: Eq + Hash, V: Clone> ChannelMap<P, V> {
    pub(crate) fn insert(&self, path: P) -> EventStream<V> {
        let mut map = self.0.lock().unwrap();
        let channel = map.entry(path).or_insert_with(|| {
            // TODO: customize the cap?
            let (send, recv) = async_broadcast::broadcast(128);
            Channel {
                send,
                recv: recv.deactivate(),
            }
        });
        EventStream(channel.recv.activate_cloned())
    }

    fn dispatch(&self, path: P, value: V) {
        let mut map = self.0.lock().unwrap();
        let Some(channel) = map.get_mut(&path) else {
            return;
        };
        if let Err(err) = channel.send.try_broadcast(value) {
            match err {
                // Should not happen, because overflow mode is enabled.
                async_broadcast::TrySendError::Full(_) => unreachable!(),
                // Should not happen, since we never close these manually.
                async_broadcast::TrySendError::Closed(_) => unreachable!(),
                // No one is listening, so just remove it completely.
                async_broadcast::TrySendError::Inactive(_) => _ = map.remove(&path),
            }
        }
    }
}

impl<P: Eq + Hash, V: Clone> Default for ChannelMap<P, V> {
    fn default() -> Self {
        Self(Mutex::new(Default::default()))
    }
}

pub(crate) struct DeviceChannels<P: DevicePath> {
    pub(crate) on_inventory_added: ChannelMap<P::Parent, P>,
    pub(crate) on_inventory_removed: ChannelMap<P::Parent, P>,
    pub(crate) on_inventory_changed: ChannelMap<P::Parent, P>,
    pub(crate) on_added: ChannelMap<P, P>,
    pub(crate) on_changed: ChannelMap<P, P>,
    pub(crate) on_removed: ChannelMap<P, P>,
}

impl<P: DevicePath> Default for DeviceChannels<P> {
    fn default() -> Self {
        Self {
            on_inventory_added: Default::default(),
            on_inventory_changed: Default::default(),
            on_inventory_removed: Default::default(),
            on_added: Default::default(),
            on_changed: Default::default(),
            on_removed: Default::default(),
        }
    }
}

impl<P: DevicePath> DeviceChannels<P> {
    fn dispatch_uevent(&self, uevent: &Uevent) {
        let Some(path) = P::parse_syspath(Utf8Path::new(uevent.devpath)) else {
            return;
        };

        match uevent.action {
            Action::Bind => {
                self.on_inventory_added.dispatch(path.parent(), path);
                self.on_added.dispatch(path, path);
            }
            Action::Change => {
                self.on_inventory_changed.dispatch(path.parent(), path);
                self.on_changed.dispatch(path, path);
            }
            Action::Unbind => {
                self.on_removed.dispatch(path, path);
                self.on_inventory_removed.dispatch(path.parent(), path);
            }
        }
    }
}

#[derive(Default)]
pub(crate) struct AllChannels {
    pub(crate) port: DeviceChannels<PortPath>,
    pub(crate) partner: DeviceChannels<PartnerPath>,
    pub(crate) plug: DeviceChannels<PlugPath>,
    pub(crate) cable: DeviceChannels<CablePath>,
    pub(crate) port_alt_mode: DeviceChannels<AltModePath<PortPath>>,
    pub(crate) partner_alt_mode: DeviceChannels<AltModePath<PartnerPath>>,
    pub(crate) plug_alt_mode: DeviceChannels<AltModePath<PlugPath>>,
    pub(crate) pd: DeviceChannels<PowerDeliveryPath>,
    // TODO: what other devices emit events? I didn't observe uevents from any
    // of the *-capabilities ones, are those just static for a pd device?
}

// Data shared between EventDispatcher *and* Watcher.
struct SharedDispatchContext {
    fd: OwnedFd,
    channels: AllChannels,
    done: AtomicBool,
    #[cfg(test)]
    enable_umockdev_events: AtomicBool,
}

pub enum EventSource {
    Netlink,
    // TODO
    // Udev,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DispatchResult {
    KeepReading,
    NoMoreEvents,
}

pub struct EventDispatcher(Arc<SharedDispatchContext>);

impl EventDispatcher {
    const UEVENT_GROUPS_KERNEL: u32 = 1;
    #[cfg(test)]
    const UEVENT_GROUPS_UDEV: u32 = 2;

    fn new(source: EventSource) -> Result<Self> {
        // TODO: udev support
        let EventSource::Netlink = source;

        let fd = socket(
            AddressFamily::Netlink,
            SockType::Raw,
            SockFlag::SOCK_CLOEXEC | SockFlag::SOCK_NONBLOCK,
            SockProtocol::NetlinkKObjectUEvent,
        )?;
        bind(
            fd.as_raw_fd(),
            &NetlinkAddr::new(0, Self::UEVENT_GROUPS_KERNEL),
        )?;
        // XXX: do we need this when always checking nl_pid?
        // setsockopt(&fd, PassCred, &true)?;
        Ok(Self(Arc::new(SharedDispatchContext {
            fd,
            channels: Default::default(),
            done: AtomicBool::new(false),
            #[cfg(test)]
            enable_umockdev_events: AtomicBool::new(false),
        })))
    }

    fn read_message(&self) -> Result<Option<Vec<u8>>> {
        // From linux/kobject.h
        const UEVENT_BUFFER_SIZE: usize = 2048;

        let mut uevent_buf = vec![0; UEVENT_BUFFER_SIZE];
        let mut cmsg_buf = cmsg_space!(UnixCredentials);

        #[cfg(test)]
        let enable_umockdev_events = self.0.enable_umockdev_events.load(Ordering::Relaxed);

        let bytes = {
            let mut iov = [IoSliceMut::new(&mut uevent_buf)];
            let msg = recvmsg::<NetlinkAddr>(
                self.0.fd.as_raw_fd(),
                &mut iov,
                Some(&mut cmsg_buf),
                MsgFlags::empty(),
            )?;

            // Ignore unknown senders.
            let Some(addr) = msg.address else {
                return Ok(None);
            };
            // XXX: do we need this when always checking nl_pid?
            // let Some(ControlMessageOwned::ScmCredentials(creds)) = msg.cmsgs()?.next() else {
            //     return Ok(None);
            // };
            if !(addr.pid() == 0
                && (addr.groups() == Self::UEVENT_GROUPS_KERNEL || {
                    #[cfg(test)]
                    {
                        // umockdev only emits uevents mocking *udev's*, so
                        // make sure to accept uevents from that source only in
                        // this scenario.
                        enable_umockdev_events && addr.groups() == Self::UEVENT_GROUPS_UDEV
                    }
                    #[cfg(not(test))]
                    false
                }))
            {
                return Ok(None);
            }

            msg.bytes
        };

        uevent_buf.drain(bytes..);

        #[cfg(test)]
        if enable_umockdev_events && uevent_buf.starts_with(b"libudev\0\xfe\xed\xca\xfe") {
            // The udev header is 40 bytes, but Uevent's parser above expects
            // at least one extra null-terminated line at the start, so just
            // drop off 39 bytes and treat the remaining one as the null.
            uevent_buf.drain(..39);
            uevent_buf[0] = 0;
        }

        Ok(Some(uevent_buf))
    }

    fn dispatch_uevent(&self, uevent: Uevent) {
        if uevent.subsystem != "typec" {
            return;
        }

        let Some(devtype) = uevent.devtype else {
            return;
        };

        match devtype {
            "typec_port" => self.0.channels.port.dispatch_uevent(&uevent),
            "typec_partner" => self.0.channels.partner.dispatch_uevent(&uevent),
            "typec_plug" => self.0.channels.plug.dispatch_uevent(&uevent),
            "typec_cable" => self.0.channels.cable.dispatch_uevent(&uevent),
            "typec_alternate_mode" => {
                // It's not clear from the uevent fields as to whether this is
                // for a port alt mode or partner alt mode, so just try both;
                // whichever can parse its path is presumably the winner.
                self.0.channels.port_alt_mode.dispatch_uevent(&uevent);
                self.0.channels.partner_alt_mode.dispatch_uevent(&uevent);
                self.0.channels.plug_alt_mode.dispatch_uevent(&uevent);
            }
            "usb_power_delivery" => self.0.channels.pd.dispatch_uevent(&uevent),
            _ => {}
        }
    }

    pub fn dispatch_pending(&self) -> Result<ControlFlow<(), ()>> {
        loop {
            if self.0.done.load(Ordering::Relaxed) {
                return Ok(ControlFlow::Break(()));
            }

            match self.read_message() {
                Ok(None) => continue,
                Ok(Some(b)) => {
                    if b.is_empty() {
                        return Ok(ControlFlow::Break(()));
                    }

                    // TODO: log errors somewhere?
                    let Ok(s) = std::str::from_utf8(&b) else {
                        continue;
                    };
                    let Ok(uevent) = Uevent::parse(s) else {
                        continue;
                    };

                    self.dispatch_uevent(uevent);
                }
                Err(Error::Io(std::io::ErrorKind::WouldBlock)) => {
                    return Ok(ControlFlow::Continue(()));
                }
                Err(err) => return Err(err),
            }
        }
    }
}

impl AsFd for EventDispatcher {
    fn as_fd(&self) -> BorrowedFd<'_> {
        self.0.fd.as_fd()
    }
}

impl AsRawFd for EventDispatcher {
    fn as_raw_fd(&self) -> RawFd {
        self.0.fd.as_raw_fd()
    }
}

// Watchers hold a weak ref to the SharedDispatchContext so that they can
// observe when the EventDispatcher has gone away and thus avoid adding new
// channels.
// All Watchers tied to a SharedDispatchContext share the *same weak reference*,
// by wrapping it in an outer Arc. This is to support cancellation in the
// *opposite* direction: when all the Watchers are dropped, then this wrapper
// will be able to shutdown the EventDispatcher.
// TODO: when we add udev support, is shutting down the socket safe? or will
// this need to use a separate channel / future to tell the EventDispatcher's
// task when to stop?
struct WatcherInner(Weak<SharedDispatchContext>);

impl Drop for WatcherInner {
    fn drop(&mut self) {
        if let Some(ctx) = self.0.upgrade() {
            ctx.done.store(true, Ordering::Relaxed);
            _ = shutdown(ctx.fd.as_raw_fd(), Shutdown::Both);
        }
    }
}

#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
#[error("watcher is no longer handling events")]
pub struct DispatcherDead;

pub type WatchResult<T> = std::result::Result<T, DispatcherDead>;

#[derive(Clone)]
pub struct Watcher(Arc<WatcherInner>);

impl Watcher {
    pub fn new_with_manual_dispatcher(source: EventSource) -> Result<(Self, EventDispatcher)> {
        let dispatcher = EventDispatcher::new(source)?;
        Ok((
            Self(Arc::new(WatcherInner(Arc::downgrade(&dispatcher.0)))),
            dispatcher,
        ))
    }

    #[cfg(feature = "tokio")]
    pub fn spawn_tokio(source: EventSource) -> Result<(Self, tokio::task::JoinHandle<Result<()>>)> {
        let (watcher, dispatcher) = Self::new_with_manual_dispatcher(source)?;

        let fd = tokio::io::unix::AsyncFd::new(dispatcher)?;

        let handle = tokio::task::spawn(async move {
            loop {
                let mut ready = fd.readable().await?;
                let res = fd.get_ref().dispatch_pending()?;
                ready.clear_ready();
                if res.is_break() {
                    return Ok::<(), Error>(());
                }
            }
        });

        Ok((watcher, handle))
    }

    pub(crate) fn with_channels<R>(&self, cb: impl FnOnce(&'_ AllChannels) -> R) -> WatchResult<R> {
        let ctx = self.0 .0.upgrade().ok_or(DispatcherDead)?;
        Ok(cb(&ctx.channels))
    }

    #[cfg(test)]
    pub fn enable_umockdev_events(&self) -> WatchResult<()> {
        let ctx = self.0 .0.upgrade().ok_or(DispatcherDead)?;
        ctx.enable_umockdev_events.store(true, Ordering::Relaxed);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use googletest::prelude::*;

    use super::*;

    #[test]
    fn uevent_parse() {
        assert_that!(
            Uevent::parse(concat!(
                "add@path\0",
                "ACTION=bind\0",
                "DEVPATH=/devices/abc\0",
                "DEVTYPE=typec_port\0",
                "SUBSYSTEM=typec\0"
            )),
            ok(pat!(&Uevent {
                action: eq(Action::Bind),
                devpath: eq("/devices/abc"),
                devtype: some(eq("typec_port")),
                subsystem: eq("typec")
            }))
        );

        assert_that!(
            Uevent::parse(concat!(
                "add@path\0",
                "ACTION=add\0",
                "DEVPATH=/devices/abc\0",
                // missing DEVTYPE
                "SUBSYSTEM=typec\0"
            )),
            err(pat!(Error::Parse))
        );
    }
}
