use std::{
    fmt::{self, Write as FmtWrite},
    fs::File,
    io::{Read, Write},
    marker::PhantomData,
    str::FromStr,
};

use camino::{Utf8Path, Utf8PathBuf};
use rustix::{
    fd::{AsFd, BorrowedFd, OwnedFd},
    fs::{openat, Dir, Mode, OFlags, CWD},
    path::Arg,
};
use strum::EnumString;

use crate::{
    types::*,
    watcher::{EventStream, WatchResult, Watcher},
    Error, Result,
};

trait PropertyReader {
    type Read;

    fn read(s: &str) -> Result<Self::Read>;
}

trait PropertyWriter: PropertyReader {
    type Write;

    fn write(dest: impl Write, value: &Self::Write) -> Result<()>;
}

pub trait PropertyReadable: fmt::Debug {
    type Read;

    fn get(&self) -> Result<Self::Read>;
}

pub trait PropertyWritable: PropertyReadable {
    type Write;

    fn set(&self, value: &Self::Write) -> Result<()>;
}

struct PropertyImpl<'fd, P: PropertyReader> {
    dfd: BorrowedFd<'fd>,
    path: &'static str,
    _impl: PhantomData<P>,
}

impl<P: PropertyReader> fmt::Debug for PropertyImpl<'_, P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(property:{})", self.path)
    }
}

impl<'fd, P: PropertyReader> PropertyImpl<'fd, P> {
    fn new(dfd: BorrowedFd<'fd>, path: &'static str) -> Self {
        Self {
            dfd,
            path,
            _impl: PhantomData,
        }
    }
}

impl<P: PropertyReader> PropertyReadable for PropertyImpl<'_, P> {
    type Read = P::Read;

    fn get(&self) -> Result<P::Read> {
        let fd = openat(
            self.dfd,
            self.path,
            OFlags::RDONLY | OFlags::CLOEXEC,
            Mode::empty(),
        )?;
        let mut file = File::from(fd);
        let mut s = "".to_owned();
        file.read_to_string(&mut s)?;
        s.truncate(s.trim_end().len());
        P::read(&s)
    }
}

impl<P: PropertyReader + PropertyWriter> PropertyWritable for PropertyImpl<'_, P> {
    type Write = P::Write;

    fn set(&self, value: &P::Write) -> Result<()> {
        let fd = openat(
            self.dfd,
            self.path,
            OFlags::RDONLY | OFlags::CLOEXEC,
            Mode::empty(),
        )?;
        P::write(File::from(fd), value)?;
        Ok(())
    }
}

struct PropertyParse<R: FromStr> {
    _phantom: PhantomData<R>,
}

impl<R: FromStr> PropertyReader for PropertyParse<R>
where
    Error: From<R::Err>,
{
    type Read = R;

    fn read(s: &str) -> Result<R> {
        s.parse().map_err(Error::from)
    }
}

struct PropertyParseDisplay<R: FromStr, W: fmt::Display> {
    _phantom: PhantomData<(R, W)>,
}

impl<R: FromStr, W: fmt::Display> PropertyReader for PropertyParseDisplay<R, W>
where
    Error: From<R::Err>,
{
    type Read = R;

    fn read(s: &str) -> Result<R> {
        PropertyParse::<R>::read(s)
    }
}

impl<R: FromStr, W: fmt::Display> PropertyWriter for PropertyParseDisplay<R, W>
where
    Error: From<R::Err>,
{
    type Write = W;

    fn write(mut dest: impl Write, value: &Self::Write) -> Result<()> {
        write!(dest, "{value}").map_err(Into::into)
    }
}

type PropertyRoleSelection<T> = PropertyParseDisplay<RoleSelection<T>, T>;

struct PropertyHexU16;

impl PropertyReader for PropertyHexU16 {
    type Read = u16;

    fn read(s: &str) -> Result<Self::Read> {
        u16::from_str_radix(s, 16).or(Err(Error::Parse))
    }
}

struct PropertyHexPrefixedU32;

impl PropertyReader for PropertyHexPrefixedU32 {
    type Read = u32;

    fn read(s: &str) -> Result<Self::Read> {
        let s = s.strip_prefix("0x").ok_or(Error::Parse)?;
        u32::from_str_radix(s, 16).or(Err(Error::Parse))
    }
}

struct PropertyBoolIntegral;

impl PropertyReader for PropertyBoolIntegral {
    type Read = bool;

    fn read(s: &str) -> Result<Self::Read> {
        let n = u32::from_str(s)?;
        Ok(n != 0)
    }
}

struct PropertyBoolYesNo;

impl PropertyReader for PropertyBoolYesNo {
    type Read = bool;

    fn read(s: &str) -> Result<Self::Read> {
        match s {
            "yes" => Ok(true),
            "no" => Ok(false),
            _ => Err(Error::Parse),
        }
    }
}

impl PropertyWriter for PropertyBoolYesNo {
    type Write = bool;

    fn write(mut dest: impl Write, value: &Self::Write) -> Result<()> {
        if *value {
            write!(dest, "yes")?;
        } else {
            write!(dest, "no")?;
        }
        Ok(())
    }
}

struct PropertyPreferredRole;

impl PropertyReader for PropertyPreferredRole {
    type Read = Option<PowerRole>;

    fn read(s: &str) -> Result<Self::Read> {
        if s.is_empty() {
            return Ok(None);
        }

        Ok(Some(s.parse()?))
    }
}

impl PropertyWriter for PropertyPreferredRole {
    type Write = Option<PowerRole>;

    fn write(mut dest: impl Write, value: &Self::Write) -> Result<()> {
        match value {
            Some(value) => write!(dest, "{value}")?,
            None => write!(dest, "none")?,
        }

        Ok(())
    }
}

macro_rules! property {
    // This is filling in the macro's parameters (function return type, source
    // filename, etc) one-by-one, most of it is just boilerplate.
    (
        _stage_fill_return,
        $name:ident,
        ro($read:ty),
        $($rest:tt)*
    ) => {
        property!(_stage_fill_with, $name,
            read($read),
            returns(impl PropertyReadable<Read = $read> + '_),
            $($rest)*);
    };

    (
        _stage_fill_return,
        $name:ident,
        rw($read:ty),
        $($rest:tt)*
    ) => {
        property!(_stage_fill_with, $name,
            read($read),
            returns(impl PropertyWritable<Read = $read, Write = $read> + '_),
            $($rest)*);
    };

    (
        _stage_fill_return,
        $name:ident,
        rw($read:ty, $write:ty),
        $($rest:tt)*
    ) => {
        property!(_stage_fill_with, $name,
            read($read),
            returns(impl PropertyWritable<Read = $read, Write = $write> + '_),
            $($rest)*);
    };

    (
        _stage_fill_with,
        $name:ident,
        read($read:ty),
        returns($ret:ty),
        with($impl:ty),
        $($rest:tt)*
    ) => {
        property!(_stage_fill_from, $name,
            returns($ret),
            with($impl),
            $($rest)*);
    };

    (
        _stage_fill_with,
        $name:ident,
        read($read:ty),
        returns($ret:ty),
        with(),
        $($rest:tt)*
    ) => {
        property!(_stage_fill_from, $name,
            returns($ret),
            with(PropertyParse::<$read>),
            $($rest)*);
    };

    (
        _stage_fill_from,
        $name:ident,
        returns($ret:ty),
        with($impl:ty),
        from($from:literal)
    ) => {
        property!(_stage_final, $name,
            returns($ret),
            with($impl),
            from($from));
    };

    (
        _stage_fill_from,
        $name:ident,
        returns($ret:ty),
        with($impl:ty),
        from(subdir($subdir:literal))
    ) => {
        property!(_stage_final, $name,
            returns($ret),
            with($impl),
            from(concat!($subdir, "/", stringify!($name))));
    };

    (
        _stage_fill_from,
        $name:ident,
        returns($ret:ty),
        with($impl:ty),
        from()
    ) => {
        property!(_stage_final, $name,
            returns($ret),
            with($impl),
            from(stringify!($name)));
    };

    (
        _stage_final,
        $name:ident,
        returns($ret:ty),
        with($impl:ty),
        from($from:expr)
    ) => {
        pub fn $name(&self) -> $ret {
            PropertyImpl::<'_, $impl>::new(self.dfd.as_fd(), $from)
        }
    };

    (
        $name:ident,
        $access:ident($read:ty $(, $write:ty)?)
        $(, with($impl:ty))?
        $(, from($($from:tt)*))?
        $(,)?
    ) => {
        property!(_stage_fill_return,
            $name,
            $access($read $(, $write)?),
            with($($impl)?),
            from($($($from)*)?));
    };
}

enum MaybeOwnedFd<'a> {
    Owned(OwnedFd),
    Borrowed(BorrowedFd<'a>),
}

impl AsFd for MaybeOwnedFd<'_> {
    fn as_fd(&self) -> BorrowedFd<'_> {
        match &self {
            MaybeOwnedFd::Owned(fd) => fd.as_fd(),
            MaybeOwnedFd::Borrowed(fd) => fd.as_fd(),
        }
    }
}

pub trait DevicePath: fmt::Debug + Copy + Clone + PartialEq + Eq + std::hash::Hash + Sized {
    type Parent: DevicePathParent;

    fn parse_basename(s: &str, parent: Self::Parent) -> Option<Self>;
    fn build_basename(&self, s: &mut String);

    fn parent(&self) -> Self::Parent;
}

macro_rules! device_path_child_collection_getter {
    ($name:ident, $ret:ty) => {
        pub fn $name(&self) -> DevicePathCollection<$ret> {
            DevicePathCollection { parent: *self }
        }
    };
}

pub trait DevicePathParent: fmt::Debug + Copy + Clone + PartialEq + Eq + std::hash::Hash {
    fn parse_syspath(p: &Utf8Path) -> Option<Self>;
    fn build_syspath(&self, p: &mut Utf8PathBuf);
}

impl<P: DevicePath> DevicePathParent for P {
    fn parse_syspath(p: &Utf8Path) -> Option<Self> {
        let parent = P::Parent::parse_syspath(p.parent()?)?;
        P::parse_basename(p.file_name()?, parent)
    }

    fn build_syspath(&self, p: &mut Utf8PathBuf) {
        self.parent().build_syspath(p);

        let mut s = "".to_owned();
        self.build_basename(&mut s);
        p.push(s);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NoParent;

impl DevicePathParent for NoParent {
    fn parse_syspath(_p: &Utf8Path) -> Option<Self> {
        Some(NoParent)
    }

    fn build_syspath(&self, _p: &mut Utf8PathBuf) {}
}

pub trait DevicePathIndexed: DevicePath {
    type Index;

    fn child_of(parent: Self::Parent, index: Self::Index) -> Self;
}

pub trait DevicePathWatchable: DevicePath {
    fn added(&self, ctx: &Watcher) -> WatchResult<EventStream<Self>>;
    fn changed(&self, ctx: &Watcher) -> WatchResult<EventStream<Self>>;
    fn removed(&self, ctx: &Watcher) -> WatchResult<EventStream<Self>>;
}

macro_rules! impl_device_path_watchable {
    ($path:ty $(, forall($($args:tt)*))?, $channels:ident) => {
        impl $($($args)*)? DevicePathWatchable for $path {
            fn added(&self, ctx: &Watcher) -> WatchResult<EventStream<Self>> {
                ctx.with_channels(|channels| channels.$channels.on_added.insert(*self))
            }
            fn changed(&self, ctx: &Watcher) -> WatchResult<EventStream<Self>> {
                ctx.with_channels(|channels| channels.$channels.on_changed.insert(*self))
            }
            fn removed(&self, ctx: &Watcher) -> WatchResult<EventStream<Self>> {
                ctx.with_channels(|channels| channels.$channels.on_removed.insert(*self))
            }
        }
    }
}

// This requires DevicePathIndexed because a singleton child is already at a fixed path
// location and thus can have added/removed invoked on itself, whereas, for an indexed
// child, we can't exactly predict what the path will be when a device gets added.
pub trait DevicePathWatchableFromParent: DevicePathIndexed {
    fn added_in(parent: Self::Parent, ctx: &Watcher) -> WatchResult<EventStream<Self>>;
    fn changed_in(parent: Self::Parent, ctx: &Watcher) -> WatchResult<EventStream<Self>>;
    fn removed_in(parent: Self::Parent, ctx: &Watcher) -> WatchResult<EventStream<Self>>;
}

macro_rules! impl_device_path_watchable_from_parent {
    ($path:ty $(, forall($($args:tt)*))?, $channels:ident) => {
        impl_device_path_watchable!($path $(, forall($($args)*))?, $channels);

        impl $($($args)*)? DevicePathWatchableFromParent for $path {
            fn added_in(parent: Self::Parent, ctx: &Watcher) -> WatchResult<EventStream<Self>> {
                ctx.with_channels(|channels| channels.$channels.on_inventory_added.insert(parent))
            }
            fn changed_in(parent: Self::Parent, ctx: &Watcher) -> WatchResult<EventStream<Self>> {
                ctx.with_channels(|channels| channels.$channels.on_inventory_changed.insert(parent))
            }
            fn removed_in(parent: Self::Parent, ctx: &Watcher) -> WatchResult<EventStream<Self>> {
                ctx.with_channels(|channels| channels.$channels.on_inventory_removed.insert(parent))
            }
        }
    }
}

pub struct DevicePathCollection<Child: DevicePath> {
    parent: Child::Parent,
}

impl<Child: DevicePath> DevicePathCollection<Child> {
    pub fn parent(&self) -> &Child::Parent {
        &self.parent
    }
}

impl<Child: DevicePathIndexed> DevicePathCollection<Child> {
    pub fn get(&self, index: Child::Index) -> Child {
        Child::child_of(self.parent, index)
    }
}

impl<Child: DevicePathWatchableFromParent> DevicePathCollection<Child> {
    pub fn added(&self, ctx: &Watcher) -> WatchResult<EventStream<Child>> {
        Child::added_in(self.parent, ctx)
    }

    pub fn changed(&self, ctx: &Watcher) -> WatchResult<EventStream<Child>> {
        Child::changed_in(self.parent, ctx)
    }
    pub fn removed(&self, ctx: &Watcher) -> WatchResult<EventStream<Child>> {
        Child::removed_in(self.parent, ctx)
    }
}

pub trait Device: Sized {
    type Path: DevicePath;

    fn from_fd(dfd: OwnedFd, path: Self::Path) -> Self;

    fn path(&self) -> &Self::Path;
}

macro_rules! impl_device {
    ($dev:ty $(, forall($($args:tt)*))?, path($path:ty)) => {
        impl $($($args)*)? Device for $dev {
            type Path = $path;

            fn from_fd(dfd: OwnedFd, path: Self::Path) -> Self {
                Self { dfd, path }
            }

            fn path(&self) -> &Self::Path {
                &self.path
            }
        }
    };
}

#[derive(Debug)]
pub struct DeviceEntry<'fd, T: Device> {
    parent_dfd: BorrowedFd<'fd>,
    path: T::Path,
}

impl<'fd, T: Device> DeviceEntry<'fd, T> {
    pub fn path(&self) -> &T::Path {
        &self.path
    }

    pub fn open(&self) -> Result<T> {
        let mut s = String::new();
        self.path.build_basename(&mut s);
        let dfd = openat(
            self.parent_dfd,
            s,
            OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            Mode::empty(),
        )?;
        Ok(T::from_fd(dfd, self.path))
    }
}

pub struct DeviceCollection<'fd, Child: Device> {
    dfd: MaybeOwnedFd<'fd>,
    parent: <Child::Path as DevicePath>::Parent,
    phantom: PhantomData<Child>,
}

impl<'fd, Child: Device> DeviceCollection<'fd, Child> {
    pub fn iter(&self) -> Result<DeviceIter<'_, Child>> {
        Ok(DeviceIter {
            dfd: self.dfd.as_fd(),
            dir: Dir::read_from(&self.dfd)?,
            parent: self.parent,
            phantom: PhantomData,
        })
    }

    pub fn iter_opened(&self) -> Result<impl Iterator<Item = Result<Child>> + '_> {
        let iter = self.iter()?;
        Ok(iter.map(|x| x.and_then(|x| x.open())))
    }

    pub fn list(&self) -> Result<Vec<DeviceEntry<'_, Child>>> {
        self.iter().and_then(|x| x.collect())
    }

    pub fn list_opened(&self) -> Result<Vec<Child>> {
        self.iter_opened().and_then(|x| x.collect())
    }
}

impl<'fd, Child: Device> DeviceCollection<'fd, Child>
where
    Child::Path: DevicePathIndexed,
{
    pub fn get(&self, index: <Child::Path as DevicePathIndexed>::Index) -> Result<Child> {
        let path = Child::Path::child_of(self.parent, index);
        let mut s = String::new();
        path.build_basename(&mut s);

        let dfd = openat(
            self.dfd.as_fd(),
            s,
            OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            Mode::empty(),
        )?;
        Ok(Child::from_fd(dfd, path))
    }
}

pub struct DeviceIter<'fd, Child: Device> {
    dfd: BorrowedFd<'fd>,
    dir: Dir,
    parent: <Child::Path as DevicePath>::Parent,
    phantom: PhantomData<Child>,
}

impl<'fd, Child: Device> Iterator for DeviceIter<'fd, Child> {
    type Item = Result<DeviceEntry<'fd, Child>>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(entry) = self.dir.next() {
            let entry = match entry {
                Ok(entry) => entry,
                Err(err) => return Some(Err(err.into())),
            };

            let name = entry.file_name();
            let name = match name.as_str() {
                Ok(name) => name,
                Err(err) => return Some(Err(err.into())),
            };

            let Some(path) = Child::Path::parse_basename(name, self.parent) else {
                continue;
            };

            if path.parent() != self.parent {
                continue;
            }

            return Some(Ok(DeviceEntry {
                parent_dfd: self.dfd,
                path,
            }));
        }

        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PortPath {
    pub port: u32,
}

impl PortPath {
    pub fn collection() -> DevicePathCollection<PortPath> {
        DevicePathCollection { parent: NoParent }
    }

    pub fn cable(&self) -> CablePath {
        CablePath { port: self.port }
    }

    pub fn partner(&self) -> PartnerPath {
        PartnerPath { port: self.port }
    }

    device_path_child_collection_getter!(alt_modes, AltModePath<PortPath>);
}

impl DevicePath for PortPath {
    type Parent = NoParent;

    fn parse_basename(s: &str, _parent: Self::Parent) -> Option<Self> {
        let s = s.strip_prefix("port")?;
        let port = u32::from_str(s).ok()?;
        Some(Self { port })
    }

    fn build_basename(&self, s: &mut String) {
        write!(s, "port{}", self.port).unwrap();
    }

    fn parent(&self) -> Self::Parent {
        NoParent
    }
}

impl DevicePathIndexed for PortPath {
    type Index = u32;

    fn child_of(_parent: Self::Parent, index: Self::Index) -> Self {
        PortPath { port: index }
    }
}

impl_device_path_watchable_from_parent!(PortPath, port);

#[derive(Debug)]
pub struct Port {
    dfd: OwnedFd,
    path: PortPath,
}

impl_device!(Port, path(PortPath));

impl Port {
    pub fn collection() -> Result<DeviceCollection<'static, Port>> {
        let dfd = openat(
            CWD,
            "/sys/class/typec",
            OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            Mode::empty(),
        )?;
        Ok(DeviceCollection {
            dfd: MaybeOwnedFd::Owned(dfd),
            parent: NoParent,
            phantom: PhantomData,
        })
    }

    property!(
        data_role,
        rw(RoleSelection<DataRole>, DataRole),
        with(PropertyRoleSelection::<DataRole>),
    );
    property!(
        port_type,
        rw(RoleSelection<PortType>, PortType),
        with(PropertyRoleSelection::<PortType>),
    );
    property!(
        power_role,
        rw(RoleSelection<PowerRole>, PowerRole),
        with(PropertyRoleSelection::<PowerRole>),
    );
    property!(
        preferred_role,
        ro(Option<PowerRole>),
        with(PropertyPreferredRole),
    );
    property!(power_operation_mode, ro(PowerOperationMode));
    property!(usb_power_delivery_revision, ro(Revision));
    property!(usb_typec_revision, ro(Revision));

    pub fn alt_modes(&self) -> DeviceCollection<'_, AltMode<PortPath>> {
        DeviceCollection {
            dfd: MaybeOwnedFd::Borrowed(self.dfd.as_fd()),
            parent: self.path,
            phantom: PhantomData,
        }
    }

    pub fn cable(&self) -> DeviceEntry<'_, Cable> {
        DeviceEntry {
            parent_dfd: self.dfd.as_fd(),
            path: self.path.cable(),
        }
    }

    pub fn partner(&self) -> DeviceEntry<'_, Partner> {
        DeviceEntry {
            parent_dfd: self.dfd.as_fd(),
            path: self.path.partner(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PartnerPath {
    pub port: u32,
}

impl PartnerPath {
    device_path_child_collection_getter!(alt_modes, AltModePath<PartnerPath>);
    device_path_child_collection_getter!(pds, PowerDeliveryPath);
}

impl DevicePath for PartnerPath {
    type Parent = PortPath;

    fn parse_basename(s: &str, parent: Self::Parent) -> Option<Self> {
        let s = s.strip_suffix("-partner")?;
        let parent = Self::Parent::parse_basename(s, parent.parent())?;
        Some(Self { port: parent.port })
    }

    fn build_basename(&self, s: &mut String) {
        self.parent().build_basename(s);
        write!(s, "-partner").unwrap();
    }

    fn parent(&self) -> Self::Parent {
        Self::Parent { port: self.port }
    }
}

impl_device_path_watchable!(PartnerPath, partner);

#[derive(Debug)]
pub struct Partner {
    dfd: OwnedFd,
    path: PartnerPath,
}

impl_device!(Partner, path(PartnerPath));

impl Partner {
    // TODO: type
    property!(usb_power_delivery_revision, ro(Revision));

    pub fn identity(&self) -> IdentityPartner<'_> {
        IdentityPartner {
            dfd: self.dfd.as_fd(),
        }
    }

    pub fn alt_modes(&self) -> DeviceCollection<'_, AltMode<PartnerPath>> {
        DeviceCollection {
            dfd: MaybeOwnedFd::Borrowed(self.dfd.as_fd()),
            parent: self.path,
            phantom: PhantomData,
        }
    }

    pub fn pds(&self) -> DeviceCollection<'_, PowerDelivery> {
        DeviceCollection {
            dfd: MaybeOwnedFd::Borrowed(self.dfd.as_fd()),
            parent: self.path,
            phantom: PhantomData,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CablePath {
    pub port: u32,
}

impl CablePath {
    device_path_child_collection_getter!(plugs, PlugPath);
}

impl DevicePath for CablePath {
    type Parent = PortPath;

    fn parse_basename(s: &str, parent: Self::Parent) -> Option<Self> {
        let s = s.strip_suffix("-cable")?;
        let parent = Self::Parent::parse_basename(s, parent.parent())?;
        Some(Self { port: parent.port })
    }

    fn build_basename(&self, s: &mut String) {
        self.parent().build_basename(s);
        write!(s, "-cable").unwrap();
    }

    fn parent(&self) -> Self::Parent {
        Self::Parent { port: self.port }
    }
}

impl_device_path_watchable!(CablePath, cable);

#[derive(Debug)]
pub struct Cable {
    dfd: OwnedFd,
    path: CablePath,
}

impl_device!(Cable, path(CablePath));

impl Cable {
    pub fn identity(&self) -> IdentityCable<'_> {
        IdentityCable {
            dfd: self.dfd.as_fd(),
        }
    }

    pub fn plugs(&self) -> DeviceCollection<'_, Plug> {
        DeviceCollection {
            dfd: MaybeOwnedFd::Borrowed(self.dfd.as_fd()),
            parent: self.path,
            phantom: PhantomData,
        }
    }

    property!(cable_type, ro(CableType), from("type"));
    property!(plug_type, ro(PlugType));
    property!(usb_power_delivery_revision, ro(Revision));
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PlugPath {
    pub port: u32,
    pub index: u32,
}

impl PlugPath {
    device_path_child_collection_getter!(alt_modes, AltModePath<PlugPath>);
}

impl DevicePath for PlugPath {
    type Parent = CablePath;

    fn parse_basename(s: &str, parent: Self::Parent) -> Option<Self> {
        let (a, b) = s.split_once('-')?;
        let parent =
            <Self::Parent as DevicePath>::Parent::parse_basename(a, parent.parent().parent())?;

        let b = b.strip_prefix("plug")?;
        let index = u32::from_str(b).ok()?;

        Some(Self {
            port: parent.port,
            index,
        })
    }

    fn build_basename(&self, s: &mut String) {
        self.parent().parent().build_basename(s);
        write!(s, "-plug{}", self.index).unwrap();
    }

    fn parent(&self) -> Self::Parent {
        Self::Parent { port: self.port }
    }
}

impl DevicePathIndexed for PlugPath {
    type Index = u32;

    fn child_of(parent: Self::Parent, index: Self::Index) -> Self {
        PlugPath {
            port: parent.port,
            index,
        }
    }
}

impl_device_path_watchable_from_parent!(PlugPath, plug);

#[derive(Debug)]
pub struct Plug {
    dfd: OwnedFd,
    path: PlugPath,
}

impl_device!(Plug, path(PlugPath));

impl Plug {
    pub fn alt_modes(&self) -> DeviceCollection<'_, AltMode<PlugPath>> {
        DeviceCollection {
            dfd: MaybeOwnedFd::Borrowed(self.dfd.as_fd()),
            parent: self.path,
            phantom: PhantomData,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AltModePath<Parent: DevicePath> {
    pub parent: Parent,
    pub index: u32,
}

impl<Parent: DevicePath> DevicePath for AltModePath<Parent> {
    type Parent = Parent;

    fn parse_basename(s: &str, parent: Self::Parent) -> Option<Self> {
        let (a, b) = s.split_once('.')?;

        let parent = Parent::parse_basename(a, parent.parent())?;
        let index = u32::from_str(b).ok()?;

        Some(AltModePath { parent, index })
    }

    fn build_basename(&self, s: &mut String) {
        self.parent().build_basename(s);
        write!(s, ".{}", self.index).unwrap();
    }

    fn parent(&self) -> Self::Parent {
        self.parent
    }
}

impl<Parent: DevicePath> DevicePathIndexed for AltModePath<Parent> {
    type Index = u32;

    fn child_of(parent: Self::Parent, index: Self::Index) -> Self {
        Self { parent, index }
    }
}

impl_device_path_watchable_from_parent!(AltModePath<PortPath>, port_alt_mode);
impl_device_path_watchable_from_parent!(AltModePath<PartnerPath>, partner_alt_mode);
impl_device_path_watchable_from_parent!(AltModePath<PlugPath>, plug_alt_mode);

#[derive(Debug)]
pub struct AltMode<Parent: DevicePath> {
    dfd: OwnedFd,
    path: AltModePath<Parent>,
}

impl_device!(AltMode<Parent>, forall(<Parent: DevicePath>), path(AltModePath<Parent>));

impl<Parent: DevicePath> AltMode<Parent> {
    property!(active, rw(bool), with(PropertyBoolYesNo));
    property!(mode, ro(u32));
    property!(svid, ro(u16), with(PropertyHexU16));
    property!(vdo, ro(u32), with(PropertyHexPrefixedU32));
}

impl AltMode<PortPath> {
    property!(supported_roles, ro(SupportedRoles));
}

#[derive(Debug)]
pub struct IdentityPartner<'fd> {
    dfd: BorrowedFd<'fd>,
}

impl IdentityPartner<'_> {
    property!(id_header, ro(VdoIdHeaderPartner), from(subdir("identity")));
    property!(cert_stat, ro(VdoCertStat), from(subdir("identity")));
    property!(product, ro(VdoProduct), from(subdir("identity")));

    // TODO: should these be different types?
    property!(
        product_type_vdo1,
        ro(u32),
        with(PropertyHexPrefixedU32),
        from(subdir("identity"))
    );
    property!(
        product_type_vdo2,
        ro(u32),
        with(PropertyHexPrefixedU32),
        from(subdir("identity"))
    );
    property!(
        product_type_vdo3,
        ro(u32),
        with(PropertyHexPrefixedU32),
        from(subdir("identity"))
    );
}

#[derive(Debug)]
pub struct IdentityCable<'fd> {
    dfd: BorrowedFd<'fd>,
}

impl IdentityCable<'_> {
    property!(id_header, ro(VdoIdHeaderCable), from(subdir("identity")));
    property!(cert_stat, ro(VdoCertStat), from(subdir("identity")));
    property!(product, ro(VdoProduct), from(subdir("identity")));

    // TODO: should these be different types?
    property!(
        product_type_vdo1,
        ro(u32),
        with(PropertyHexPrefixedU32),
        from(subdir("identity"))
    );
    property!(
        product_type_vdo2,
        ro(u32),
        with(PropertyHexPrefixedU32),
        from(subdir("identity"))
    );
    property!(
        product_type_vdo3,
        ro(u32),
        with(PropertyHexPrefixedU32),
        from(subdir("identity"))
    );
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumString, strum::Display)]
#[strum(serialize_all = "snake_case")]
pub enum SupplyKind {
    FixedSupply,
    Battery,
    VariableSupply,
    ProgrammableSupply,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PowerDeliveryPath {
    pub port: u32,
    pub pd: u32,
}

impl PowerDeliveryPath {
    pub fn source_capabilities(&self) -> CapabilitiesPath {
        CapabilitiesPath {
            port: self.port,
            pd: self.pd,
            role: PowerRole::Source,
        }
    }

    pub fn sink_capabilities(&self) -> CapabilitiesPath {
        CapabilitiesPath {
            port: self.port,
            pd: self.pd,
            role: PowerRole::Sink,
        }
    }
}

impl DevicePath for PowerDeliveryPath {
    type Parent = PartnerPath;

    fn parse_basename(s: &str, parent: Self::Parent) -> Option<Self> {
        let s = s.strip_prefix("pd")?;
        let pd = u32::from_str(s).ok()?;

        Some(Self {
            port: parent.port,
            pd,
        })
    }

    fn build_basename(&self, s: &mut String) {
        write!(s, "pd{}", self.pd).unwrap();
    }

    fn parent(&self) -> Self::Parent {
        Self::Parent { port: self.port }
    }
}

impl DevicePathIndexed for PowerDeliveryPath {
    type Index = u32;

    fn child_of(parent: Self::Parent, index: Self::Index) -> Self {
        Self {
            port: parent.port,
            pd: index,
        }
    }
}

impl_device_path_watchable_from_parent!(PowerDeliveryPath, pd);

#[derive(Debug)]
pub struct PowerDelivery {
    dfd: OwnedFd,
    path: PowerDeliveryPath,
}

impl_device!(PowerDelivery, path(PowerDeliveryPath));

impl PowerDelivery {
    pub fn source_capabilities(&self) -> DeviceEntry<'_, SourceCapabilities> {
        DeviceEntry {
            parent_dfd: self.dfd.as_fd(),
            path: self.path.source_capabilities(),
        }
    }

    pub fn sink_capabilities(&self) -> DeviceEntry<'_, SinkCapabilities> {
        DeviceEntry {
            parent_dfd: self.dfd.as_fd(),
            path: self.path.sink_capabilities(),
        }
    }
}

// TODO: split this into source/sink so we can add pdos()
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CapabilitiesPath {
    pub port: u32,
    pub pd: u32,
    pub role: PowerRole,
}

impl DevicePath for CapabilitiesPath {
    type Parent = PowerDeliveryPath;

    fn parse_basename(s: &str, parent: Self::Parent) -> Option<Self> {
        let s = s.strip_suffix("-capabilities")?;
        let role = PowerRole::from_str(s).ok()?;
        Some(Self {
            port: parent.port,
            pd: parent.pd,
            role,
        })
    }

    fn build_basename(&self, s: &mut String) {
        write!(s, "{}-capabilities", self.role).unwrap();
    }

    fn parent(&self) -> Self::Parent {
        Self::Parent {
            port: self.port,
            pd: self.pd,
        }
    }
}

#[derive(Debug)]
pub struct SourceCapabilities {
    dfd: OwnedFd,
    path: CapabilitiesPath,
}

impl_device!(SourceCapabilities, path(CapabilitiesPath));

impl SourceCapabilities {
    pub fn pdos(&self) -> DeviceCollection<'_, SourcePdo> {
        DeviceCollection {
            dfd: MaybeOwnedFd::Borrowed(self.dfd.as_fd()),
            parent: self.path,
            phantom: PhantomData,
        }
    }
}

// XXX: copy-pasted struct, but making this generic like AltMode is hard
#[derive(Debug)]
pub struct SinkCapabilities {
    dfd: OwnedFd,
    path: CapabilitiesPath,
}

impl_device!(SinkCapabilities, path(CapabilitiesPath));

impl SinkCapabilities {
    pub fn pdos(&self) -> DeviceCollection<'_, SinkPdo> {
        DeviceCollection {
            dfd: MaybeOwnedFd::Borrowed(self.dfd.as_fd()),
            parent: self.path,
            phantom: PhantomData,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PdoPath {
    pub port: u32,
    pub pd: u32,

    pub role: PowerRole,
    pub index: u32,
    pub supply: SupplyKind,
}

impl DevicePath for PdoPath {
    type Parent = CapabilitiesPath;

    fn parse_basename(s: &str, parent: Self::Parent) -> Option<Self> {
        let (a, b) = s.split_once(':')?;
        let index = u32::from_str(a).ok()?;
        let supply = SupplyKind::from_str(b).ok()?;
        Some(Self {
            port: parent.port,
            pd: parent.pd,
            role: parent.role,
            index,
            supply,
        })
    }

    fn build_basename(&self, s: &mut String) {
        write!(s, "{}:{}", self.index, self.supply).unwrap();
    }

    fn parent(&self) -> Self::Parent {
        Self::Parent {
            port: self.port,
            pd: self.pd,
            role: self.role,
        }
    }
}

#[derive(Debug)]
pub enum SourcePdo {
    FixedSupply(SourcePdoFixedSupply),
    Battery(SourcePdoBattery),
    VariableSupply(SourcePdoVariableSupply),
    ProgrammableSupply(SourcePdoProgrammableSupply),
}

impl Device for SourcePdo {
    type Path = PdoPath;

    fn from_fd(dfd: OwnedFd, path: Self::Path) -> Self {
        match path.supply {
            SupplyKind::FixedSupply => SourcePdo::FixedSupply(SourcePdoFixedSupply { dfd, path }),
            SupplyKind::Battery => SourcePdo::Battery(SourcePdoBattery { dfd, path }),
            SupplyKind::VariableSupply => {
                SourcePdo::VariableSupply(SourcePdoVariableSupply { dfd, path })
            }
            SupplyKind::ProgrammableSupply => {
                SourcePdo::ProgrammableSupply(SourcePdoProgrammableSupply { dfd, path })
            }
        }
    }

    fn path(&self) -> &Self::Path {
        match self {
            SourcePdo::FixedSupply(pdo) => pdo.path(),
            SourcePdo::Battery(pdo) => pdo.path(),
            SourcePdo::VariableSupply(pdo) => pdo.path(),
            SourcePdo::ProgrammableSupply(pdo) => pdo.path(),
        }
    }
}

#[derive(Debug)]
pub enum SinkPdo {
    FixedSupply(SinkPdoFixedSupply),
    Battery(SinkPdoBattery),
    VariableSupply(SinkPdoVariableSupply),
    ProgrammableSupply(SinkPdoProgrammableSupply),
}

impl Device for SinkPdo {
    type Path = PdoPath;

    fn from_fd(dfd: OwnedFd, path: Self::Path) -> Self {
        match path.supply {
            SupplyKind::FixedSupply => SinkPdo::FixedSupply(SinkPdoFixedSupply { dfd, path }),
            SupplyKind::Battery => SinkPdo::Battery(SinkPdoBattery { dfd, path }),
            SupplyKind::VariableSupply => {
                SinkPdo::VariableSupply(SinkPdoVariableSupply { dfd, path })
            }
            SupplyKind::ProgrammableSupply => {
                SinkPdo::ProgrammableSupply(SinkPdoProgrammableSupply { dfd, path })
            }
        }
    }

    fn path(&self) -> &Self::Path {
        match self {
            SinkPdo::FixedSupply(pdo) => pdo.path(),
            SinkPdo::Battery(pdo) => pdo.path(),
            SinkPdo::VariableSupply(pdo) => pdo.path(),
            SinkPdo::ProgrammableSupply(pdo) => pdo.path(),
        }
    }
}

#[derive(Debug)]
pub struct SourcePdoFixedSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SourcePdoFixedSupply, path(PdoPath));

impl SourcePdoFixedSupply {
    // first item only!
    property!(dual_role_power, ro(bool), with(PropertyBoolIntegral));
    property!(usb_suspend_supported, ro(bool), with(PropertyBoolIntegral));
    property!(unconstrained_power, ro(bool), with(PropertyBoolIntegral));
    property!(
        usb_communication_capable,
        ro(bool),
        with(PropertyBoolIntegral)
    );
    property!(dual_role_data, ro(bool), with(PropertyBoolIntegral));
    property!(
        unchunked_extended_messages_supported,
        ro(bool),
        with(PropertyBoolIntegral)
    );

    // available on all items
    property!(peak_current, ro(u32));
    property!(voltage, ro(Millivolts));
    property!(maximum_current, ro(Milliamps));
}

#[derive(Debug)]
pub struct SinkPdoFixedSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SinkPdoFixedSupply, path(PdoPath));

impl SinkPdoFixedSupply {
    // first item only!
    property!(dual_role_power, ro(bool), with(PropertyBoolIntegral));
    property!(higher_capability, ro(bool), with(PropertyBoolIntegral));
    property!(unconstrained_power, ro(bool), with(PropertyBoolIntegral));
    property!(
        usb_communication_capable,
        ro(bool),
        with(PropertyBoolIntegral)
    );
    property!(dual_role_data, ro(bool), with(PropertyBoolIntegral));
    property!(
        unchunked_extended_messages_supported,
        ro(bool),
        with(PropertyBoolIntegral)
    );
    property!(fast_role_swap_current, ro(u32));

    // available on all items
    property!(voltage, ro(Millivolts));
    property!(operational_current, ro(Milliamps));
}

#[derive(Debug)]
pub struct SourcePdoBattery {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SourcePdoBattery, path(PdoPath));

impl SourcePdoBattery {
    property!(maximum_voltage, ro(Millivolts));
    property!(minimum_voltage, ro(Millivolts));
    property!(maximum_power, ro(Milliwatts));
}

#[derive(Debug)]
pub struct SinkPdoBattery {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SinkPdoBattery, path(PdoPath));

impl SinkPdoBattery {
    property!(maximum_voltage, ro(Millivolts));
    property!(minimum_voltage, ro(Millivolts));
    property!(operational_power, ro(Milliwatts));
}

#[derive(Debug)]
pub struct SourcePdoVariableSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SourcePdoVariableSupply, path(PdoPath));

impl SourcePdoVariableSupply {
    property!(maximum_voltage, ro(Millivolts));
    property!(minimum_voltage, ro(Millivolts));
    property!(maximum_current, ro(Milliamps));
}

#[derive(Debug)]
pub struct SinkPdoVariableSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SinkPdoVariableSupply, path(PdoPath));

impl SinkPdoVariableSupply {
    property!(maximum_voltage, ro(Millivolts));
    property!(minimum_voltage, ro(Millivolts));
    property!(operational_current, ro(Milliamps));
}

#[derive(Debug)]
pub struct SourcePdoProgrammableSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SourcePdoProgrammableSupply, path(PdoPath));

impl SourcePdoProgrammableSupply {
    property!(power_limited, ro(bool), with(PropertyBoolIntegral));
    property!(maximum_voltage, ro(Millivolts));
    property!(minimum_voltage, ro(Millivolts));
    property!(maximum_current, ro(Milliamps));
}

#[derive(Debug)]
pub struct SinkPdoProgrammableSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SinkPdoProgrammableSupply, path(PdoPath));

impl SinkPdoProgrammableSupply {
    property!(maximum_voltage, ro(Millivolts));
    property!(minimum_voltage, ro(Millivolts));
    property!(maximum_current, ro(Milliamps));
}
