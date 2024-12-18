#![doc = include_str!("../../docs/watcher.md")]

use std::{
    collections::HashMap,
    fmt,
    future::Future,
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
use event_listener::Event;
use futures_core::Stream;
use nix::{
    cmsg_space,
    sys::socket::{
        bind, recvmsg, socket, AddressFamily, MsgFlags, NetlinkAddr, SockFlag, SockProtocol,
        SockType, UnixCredentials,
    },
};
use rustix::fd::{AsFd, AsRawFd, BorrowedFd, OwnedFd, RawFd};
use strum::EnumString;

use crate::{sysfs::*, Error, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString)]
#[strum(serialize_all = "lowercase")]
enum Action {
    // No Bind or Unbind, because we don't really care about those events here,
    // so failing to parse is fine.
    Add,
    Change,
    Remove,
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

/// An error returned when a [`DevicePathStream`] has too many unread events,
/// forcing some to be dropped.
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
#[error("stream overflowed, missed {missed_events} events")]
pub struct Overflowed {
    pub missed_events: u64,
}

/// An async stream of [`DevicePath`]s.
pub struct DevicePathStream<P: DevicePathParent, V: DevicePath> {
    watched_path: P,
    recv: async_broadcast::Receiver<V>,
    _inner: Arc<WatcherInner>,
}

impl<P: DevicePathParent, V: DevicePath> DevicePathStream<P, V> {
    /// Returns the path being watched.
    ///
    /// This does not necessarily correspond with the path in the sent events,
    /// e.g. when watching a parent for new children, this will return the
    /// parent path, while the stream will return the child paths.
    pub fn watched_path(&self) -> &P {
        &self.watched_path
    }
}

impl<P: DevicePathParent, V: DevicePath> fmt::Debug for DevicePathStream<P, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DevicePathStream")
            .field("watched_path", &self.watched_path)
            .field("recv", &self.recv)
            .finish()
    }
}

impl<P: DevicePathParent, V: DevicePath> Stream for DevicePathStream<P, V> {
    type Item = std::result::Result<V, Overflowed>;

    fn poll_next(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        Pin::new(&mut self.recv)
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

pub(crate) struct ChannelMap<P: Eq + Hash, V: DevicePath>(Mutex<HashMap<P, Channel<V>>>);

impl<P: DevicePathParent, V: DevicePath> ChannelMap<P, V> {
    pub(crate) fn insert(&self, path: P, inner: &Arc<WatcherInner>) -> DevicePathStream<P, V> {
        let mut map = self.0.lock().unwrap();
        let channel = map.entry(path).or_insert_with(|| {
            // TODO: customize the cap?
            let (send, mut recv) = async_broadcast::broadcast(128);
            recv.set_overflow(true);
            Channel {
                send,
                recv: recv.deactivate(),
            }
        });
        DevicePathStream {
            watched_path: path,
            recv: channel.recv.activate_cloned(),
            _inner: inner.clone(),
        }
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

impl<P: Eq + Hash, V: DevicePath> Default for ChannelMap<P, V> {
    fn default() -> Self {
        Self(Mutex::new(Default::default()))
    }
}

pub(crate) struct DeviceChannels<P: DevicePath> {
    pub(crate) on_any_added: ChannelMap<NoParent, P>,
    pub(crate) on_any_removed: ChannelMap<NoParent, P>,
    pub(crate) on_any_changed: ChannelMap<NoParent, P>,
    pub(crate) on_inventory_added: ChannelMap<P::Parent, P>,
    pub(crate) on_inventory_changed: ChannelMap<P::Parent, P>,
    pub(crate) on_inventory_removed: ChannelMap<P::Parent, P>,
    pub(crate) on_added: ChannelMap<P, P>,
    pub(crate) on_changed: ChannelMap<P, P>,
    pub(crate) on_removed: ChannelMap<P, P>,
}

impl<P: DevicePath> Default for DeviceChannels<P> {
    fn default() -> Self {
        Self {
            on_any_added: Default::default(),
            on_any_changed: Default::default(),
            on_any_removed: Default::default(),
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
            Action::Add => {
                self.on_any_added.dispatch(NoParent, path);
                self.on_inventory_added.dispatch(path.parent(), path);
                self.on_added.dispatch(path, path);
            }
            Action::Change => {
                self.on_any_changed.dispatch(NoParent, path);
                self.on_inventory_changed.dispatch(path.parent(), path);
                self.on_changed.dispatch(path, path);
            }
            Action::Remove => {
                self.on_removed.dispatch(path, path);
                self.on_inventory_removed.dispatch(path.parent(), path);
                self.on_any_removed.dispatch(NoParent, path);
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
    done_event: Event,
    enable_umockdev_events_for_testing: AtomicBool,
}

/// Source to read device events from.
///
/// Currently, only netlink is supported.
pub enum EventSource {
    Netlink,
    // TODO
    // Udev,
}

/// A dispatcher for device events.
///
/// When writing a custom main loop integration, you should poll this
/// structure's fd (via [`AsFd::as_fd`] or [`AsRawFd::as_raw_fd`]) until either
/// it's readable or the future returned by [`EventDispatcher::wait_exit`]
/// returns. If it's readable, then call `EventDispatcher::dispatch_pending`,
/// looping again if [`ControlFlow::Continue`] is returned.
///
/// Obtainable via [`Watcher::new_with_manual_dispatcher`].
pub struct EventDispatcher(Arc<SharedDispatchContext>);

impl EventDispatcher {
    const UEVENT_GROUPS_KERNEL: u32 = 1;
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
            done_event: Event::new(),
            enable_umockdev_events_for_testing: AtomicBool::new(false),
        })))
    }

    fn read_message(&self) -> Result<Option<Vec<u8>>> {
        // From linux/kobject.h
        const UEVENT_BUFFER_SIZE: usize = 2048;

        let mut uevent_buf = vec![0; UEVENT_BUFFER_SIZE];
        let mut cmsg_buf = cmsg_space!(UnixCredentials);

        let enable_umockdev = self
            .0
            .enable_umockdev_events_for_testing
            .load(Ordering::Relaxed);

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
                && (addr.groups() == Self::UEVENT_GROUPS_KERNEL
                    || (
                        // umockdev only emits uevents mocking *udev's*, so
                        // make sure to accept uevents from that source only in
                        // this scenario.
                        enable_umockdev && addr.groups() == Self::UEVENT_GROUPS_UDEV
                    )))
            {
                return Ok(None);
            }

            msg.bytes
        };

        uevent_buf.drain(bytes..);

        if enable_umockdev && uevent_buf.starts_with(b"libudev\0\xfe\xed\xca\xfe") {
            // The udev header is 40 bytes, but Uevent's parser above expects
            // at least one extra null-terminated line at the start, so just
            // drop off 39 bytes and treat the remaining one as the null.
            uevent_buf.drain(..39);
            uevent_buf[0] = 0;
        }

        Ok(Some(uevent_buf))
    }

    fn dispatch_uevent(&self, uevent: Uevent) {
        if !matches!(uevent.subsystem, "typec" | "usb_power_delivery") {
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
                self.0.channels.partner_alt_mode.dispatch_uevent(&uevent);
                self.0.channels.plug_alt_mode.dispatch_uevent(&uevent);
            }
            "usb_power_delivery" => self.0.channels.pd.dispatch_uevent(&uevent),
            _ => {}
        }
    }

    /// Returns a future that will resolve when all the [`Watcher`]s tied to
    /// this dispatcher are gone.
    ///
    /// Once this future is ready, the caller should exit.
    pub fn wait_exit(&self) -> Pin<Box<dyn Future<Output = ()> + Send>> {
        let listen = self.0.done_event.listen();

        if self.0.done.load(Ordering::Acquire) {
            Box::pin(std::future::ready(()))
        } else {
            Box::pin(listen)
        }
    }

    /// Dispatches all pending events.
    ///
    /// Returns [`ControlFlow::Continue`] if the caller should keep polling
    /// the fd or [`ControlFlow::Break`] if the caller should exit.
    pub fn dispatch_pending(&self) -> Result<ControlFlow<(), ()>> {
        loop {
            if self.0.done.load(Ordering::Acquire) {
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
// will be able to tell the EventDispatcher to stop.
pub(crate) struct WatcherInner(Weak<SharedDispatchContext>);

impl Drop for WatcherInner {
    fn drop(&mut self) {
        if let Some(ctx) = self.0.upgrade() {
            ctx.done.store(true, Ordering::Release);
            ctx.done_event.notify(usize::MAX);
        }
    }
}

/// An error returned when the [`EventDispatcher`] backing a [`Watcher`] has
/// died, resulting in the [`Watcher`] being unable to process any more events.
#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
#[error("watcher is no longer handling events")]
pub struct DispatcherDead;

pub type WatchResult<T> = std::result::Result<T, DispatcherDead>;

/// A shared context used to listen for device events.
///
/// For Tokio users, use [`Watcher::spawn_tokio`] to spawn a background task
/// that listens for events and dispatches them. Otherwise, use
/// [`Watcher::new_with_manual_dispatcher`] to obtain an [`EventDispatcher`]
/// that can be used in the background via your desired runtime.
///
/// Once a [`Watcher`] is obtained, it can be passed to the relevant methods on
/// [`DevicePathWatchable`] and [`DevicePathCollection`] to obtain a
/// [`DevicePathStream`].
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

    /// Returns a [`Watcher`] backed by a Tokio background task, as well as the
    /// [`tokio::task::JoinHandle`] for the task.
    #[cfg(feature = "tokio")]
    #[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
    pub fn spawn_tokio(source: EventSource) -> Result<(Self, tokio::task::JoinHandle<Result<()>>)> {
        let (watcher, dispatcher) = Self::new_with_manual_dispatcher(source)?;

        let fd = tokio::io::unix::AsyncFd::new(dispatcher)?;

        let handle = tokio::task::spawn(async move {
            let mut done = fd.get_ref().wait_exit();

            loop {
                let mut ready = tokio::select! {
                    ready = fd.readable() => ready?,
                    _ = &mut done => break,
                };
                let res = fd.get_ref().dispatch_pending()?;
                ready.clear_ready();
                if res.is_break() {
                    break;
                }
            }

            Ok::<(), Error>(())
        });

        Ok((watcher, handle))
    }

    pub(crate) fn with_channels<R>(
        &self,
        cb: impl FnOnce(&'_ AllChannels, &'_ Arc<WatcherInner>) -> R,
    ) -> WatchResult<R> {
        let ctx = self.0 .0.upgrade().ok_or(DispatcherDead)?;
        Ok(cb(&ctx.channels, &self.0))
    }

    #[doc(hidden)]
    pub fn enable_umockdev_events_for_testing(&self) -> WatchResult<()> {
        let ctx = self.0 .0.upgrade().ok_or(DispatcherDead)?;
        ctx.enable_umockdev_events_for_testing
            .store(true, Ordering::Relaxed);
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
                "ACTION=add\0",
                "DEVPATH=/devices/abc\0",
                "DEVTYPE=typec_port\0",
                "SUBSYSTEM=typec\0"
            )),
            ok(pat!(&Uevent {
                action: eq(Action::Add),
                devpath: eq("/devices/abc"),
                devtype: some(eq("typec_port")),
                subsystem: eq("typec")
            }))
        );

        assert_that!(
            Uevent::parse(concat!(
                "add@path\0",
                "ACTION=add\0",
                // missing DEVPATH, DEVTYPE
                "SUBSYSTEM=typec\0"
            )),
            err(pat!(Error::Parse))
        );
    }

    #[test]
    fn dispatcher_death() {
        let (w, ed) = Watcher::new_with_manual_dispatcher(EventSource::Netlink).unwrap();
        std::mem::drop(ed);

        assert_that!(PortPath { port: 0 }.added(&w), err(eq(&DispatcherDead)));
    }

    #[cfg(feature = "tokio")]
    #[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
    #[tokio::test]
    async fn watcher_death() {
        use std::time::Duration;

        use tokio::time::{sleep, timeout};

        const TIMEOUT: Duration = Duration::from_millis(25);

        let (w, mut jh) = Watcher::spawn_tokio(EventSource::Netlink).unwrap();
        let w1 = w.clone();
        let added = PortPath { port: 0 }.added(&w);

        sleep(TIMEOUT).await;

        std::mem::drop(w);
        std::mem::drop(w1);

        assert_that!(timeout(TIMEOUT, &mut jh).await, err(anything()));

        std::mem::drop(added);
        assert_that!(timeout(TIMEOUT, &mut jh).await, ok(ok(ok(()))));
    }
}
