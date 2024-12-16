//! Accessing the sysfs.

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
    watcher::{DevicePathStream, WatchResult, Watcher},
    Error, Result,
};

mod sealed {
    pub trait Sealed {}
}

use sealed::Sealed;

macro_rules! impl_sealed {
    ($ty:ty $(, forall($($args:tt)*))?) => {
        impl $($($args)*)? Sealed for $ty {}
    };
}

trait PropertyReader {
    type Read;

    fn read(s: &str) -> Result<Self::Read>;
}

trait PropertyWriter: PropertyReader {
    type Write;

    fn write(dest: impl Write, value: &Self::Write) -> Result<()>;
}

/// A readable sysfs property.
pub trait PropertyReadable: Sealed + fmt::Debug {
    type Read;

    // Reads the given property from the sysfs.
    fn get(&self) -> Result<Self::Read>;
}

// A readable and writable sysfs property.
pub trait PropertyWritable: PropertyReadable {
    type Write;

    // Sets the given property on the sysfs to the value.
    fn set(&self, value: &Self::Write) -> Result<()>;
}

struct PropertyImpl<'fd, P: PropertyReader> {
    dfd: BorrowedFd<'fd>,
    path: &'static str,
    _impl: PhantomData<P>,
}

impl_sealed!(PropertyImpl<'_, P>, forall(<P: PropertyReader>));

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
        from($from:literal),
        $($rest:tt)*
    ) => {
        property!(_stage_final, $name,
            returns($ret),
            with($impl),
            from($from),
            $($rest)*);
    };

    (
        _stage_fill_from,
        $name:ident,
        returns($ret:ty),
        with($impl:ty),
        from(subdir($subdir:literal)),
        $($rest:tt)*
    ) => {
        property!(_stage_final, $name,
            returns($ret),
            with($impl),
            from(concat!($subdir, "/", stringify!($name))),
            $($rest)*);
    };

    (
        _stage_fill_from,
        $name:ident,
        returns($ret:ty),
        with($impl:ty),
        from(),
        $($rest:tt)*
    ) => {
        property!(_stage_final, $name,
            returns($ret),
            with($impl),
            from(stringify!($name)),
            $($rest)*);
    };

    (
        _stage_final,
        $name:ident,
        returns($ret:ty),
        with($impl:ty),
        from($from:expr),
        doc($($doc:tt)?)
    ) => {
        $(#[doc = $doc])?
        pub fn $name(&self) -> $ret {
            PropertyImpl::<'_, $impl>::new(self.dfd.as_fd(), $from)
        }
    };

    (
        $name:ident,
        $access:ident($read:ty $(, $write:ty)?)
        $(, with($impl:ty))?
        $(, from($($from:tt)*))?
        $(, doc($doc:tt))?
        $(,)?
    ) => {
        property!(_stage_fill_return,
            $name,
            $access($read $(, $write)?),
            with($($impl)?),
            from($($($from)*)?),
            doc($($doc)?));
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

/// A reference to the location of a [`Device`] on the sysfs.
///
/// Rather than being an actual filesystem path, this simply contains
/// information like "what is the index of the device", which, when combined
/// with the type itself, gives it the ability to parse or construct actual
/// paths on-the-fly as needed.
///
/// Note that these can be constructed at will and thus may not actually
/// correspond to an existing device! It's simply a reference to a filesystem
/// location that can *potentially contain* a device.
pub trait DevicePath:
    Sealed + fmt::Debug + Copy + Clone + PartialEq + Eq + std::hash::Hash + Sized + Unpin
{
    /// The parent of this path, i.e. the [`DevicePath`] representing the
    /// filesystem location that *contains* this device. If this device is not
    /// nested within another, then this will be [`NoParent`].
    type Parent: DevicePathParent;

    fn parse_basename(s: &str, parent: Self::Parent) -> Option<Self>;
    fn build_basename(&self, s: &mut String);

    fn parent(&self) -> Self::Parent;
}

macro_rules! device_path_child_collection_getter {
    ($name:ident, $ret:ty $(, doc($doc:tt))? $(,)?) => {
        $(#[doc = $doc])?
        pub fn $name(&self) -> DevicePathCollection<$ret> {
            DevicePathCollection { parent: *self }
        }
    };
}

/// A parent of a [`DevicePath`]. This can be either a [`DevicePath`] itself or
/// [`NoParent`].
pub trait DevicePathParent:
    Sealed + fmt::Debug + Copy + Clone + PartialEq + Eq + std::hash::Hash + Unpin
{
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

/// A stub type used as a [`DevicePathParent`] for when a [`Device`] doesn't
/// actually have a parent.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NoParent;

impl_sealed!(NoParent);

impl DevicePathParent for NoParent {
    fn parse_syspath(_p: &Utf8Path) -> Option<Self> {
        Some(NoParent)
    }

    fn build_syspath(&self, _p: &mut Utf8PathBuf) {}
}

/// A child [`DevicePath`] that can be present in its parent multiple times.
pub trait DevicePathIndexed: DevicePath {
    type Index;

    /// Prefer using [`DevicePathCollection.get`].
    fn child_of(parent: Self::Parent, index: Self::Index) -> Self;
}

/// A [`DevicePath`] that can be watched for add/change/remove events.
pub trait DevicePathWatchable: DevicePath {
    /// Returns a stream of all [`DevicePath`]s of this type that are added.
    fn any_added(ctx: &Watcher) -> WatchResult<DevicePathStream<NoParent, Self>>;
    /// Returns a stream of all [`DevicePath`]s of this type that are changed.
    fn any_changed(ctx: &Watcher) -> WatchResult<DevicePathStream<NoParent, Self>>;
    /// Returns a stream of all [`DevicePath`]s of this type that are removed.
    fn any_removed(ctx: &Watcher) -> WatchResult<DevicePathStream<NoParent, Self>>;

    /// Returns a stream containing only this [`DevicePath`] whenever it's
    /// added.
    fn added(&self, ctx: &Watcher) -> WatchResult<DevicePathStream<Self, Self>>;
    /// Returns a stream containing only this [`DevicePath`] whenever it's
    /// changed.
    fn changed(&self, ctx: &Watcher) -> WatchResult<DevicePathStream<Self, Self>>;
    /// Returns a stream containing only this [`DevicePath`] whenever it's
    /// removed.
    fn removed(&self, ctx: &Watcher) -> WatchResult<DevicePathStream<Self, Self>>;
}

macro_rules! impl_device_path_watchable {
    ($path:ty $(, forall($($args:tt)*))?, $channels:ident) => {
        impl $($($args)*)? DevicePathWatchable for $path {
            fn any_added(ctx: &Watcher) -> WatchResult<DevicePathStream<NoParent, Self>> {
                ctx.with_channels(|channels, inner| channels.$channels.on_any_added.insert(NoParent, inner))
            }
            fn any_changed(ctx: &Watcher) -> WatchResult<DevicePathStream<NoParent, Self>> {
                ctx.with_channels(|channels, inner| channels.$channels.on_any_changed.insert(NoParent, inner))
            }
            fn any_removed(ctx: &Watcher) -> WatchResult<DevicePathStream<NoParent, Self>> {
                ctx.with_channels(|channels, inner| channels.$channels.on_any_removed.insert(NoParent, inner))
            }

            fn added(&self, ctx: &Watcher) -> WatchResult<DevicePathStream<Self, Self>> {
                ctx.with_channels(|channels, inner| channels.$channels.on_added.insert(*self, inner))
            }
            fn changed(&self, ctx: &Watcher) -> WatchResult<DevicePathStream<Self, Self>> {
                ctx.with_channels(|channels, inner| channels.$channels.on_changed.insert(*self, inner))
            }
            fn removed(&self, ctx: &Watcher) -> WatchResult<DevicePathStream<Self, Self>> {
                ctx.with_channels(|channels, inner| channels.$channels.on_removed.insert(*self, inner))
            }
        }
    }
}

/// A [`DevicePathIndexed`] that can be watched by invoking one of
/// [`DevicePathCollection::added`], [`DevicePathCollection::changed`], or
/// [`DevicePathCollection::removed`] on the parent's corresponding
/// [`DevicePathCollection`].
///
/// This requires [`DevicePathIndexed`] because a singleton child is already at a fixed path
/// location and thus can have added/removed invoked on itself, whereas, for an indexed
/// child, we can't exactly predict what the path will be when a device gets added.
pub trait DevicePathWatchableFromParent: DevicePathIndexed {
    /// Prefer using [`DevicePathCollection.added`].
    fn added_in(
        parent: Self::Parent,
        ctx: &Watcher,
    ) -> WatchResult<DevicePathStream<Self::Parent, Self>>;
    /// Prefer using [`DevicePathCollection.changed`].
    fn changed_in(
        parent: Self::Parent,
        ctx: &Watcher,
    ) -> WatchResult<DevicePathStream<Self::Parent, Self>>;
    /// Prefer using [`DevicePathCollection.removed`].
    fn removed_in(
        parent: Self::Parent,
        ctx: &Watcher,
    ) -> WatchResult<DevicePathStream<Self::Parent, Self>>;
}

macro_rules! impl_device_path_watchable_from_parent {
    ($path:ty $(, forall($($args:tt)*))?, $channels:ident) => {
        impl_device_path_watchable!($path $(, forall($($args)*))?, $channels);

        impl $($($args)*)? DevicePathWatchableFromParent for $path {
            fn added_in(parent: Self::Parent, ctx: &Watcher) -> WatchResult<DevicePathStream<Self::Parent, Self>> {
                ctx.with_channels(|channels, inner| channels.$channels.on_inventory_added.insert(parent, inner))
            }
            fn changed_in(parent: Self::Parent, ctx: &Watcher) -> WatchResult<DevicePathStream<Self::Parent, Self>> {
                ctx.with_channels(|channels, inner| channels.$channels.on_inventory_changed.insert(parent, inner))
            }
            fn removed_in(parent: Self::Parent, ctx: &Watcher) -> WatchResult<DevicePathStream<Self::Parent, Self>> {
                ctx.with_channels(|channels, inner| channels.$channels.on_inventory_removed.insert(parent, inner))
            }
        }
    }
}

/// A wrapper over a parent that can be used to get child paths of a given type.
pub struct DevicePathCollection<Child: DevicePath> {
    parent: Child::Parent,
}

impl<Child: DevicePath> DevicePathCollection<Child> {
    /// Returns the parent containing the child paths.
    pub fn parent(&self) -> &Child::Parent {
        &self.parent
    }
}

impl<Child: DevicePathIndexed> DevicePathCollection<Child> {
    /// Returns a path for the child at the given index.
    pub fn get(&self, index: Child::Index) -> Child {
        Child::child_of(self.parent, index)
    }
}

impl<Child: DevicePathWatchableFromParent> DevicePathCollection<Child> {
    /// Returns a stream of [`DevicePath`]s that are added and children of
    /// this collection's parent path.
    pub fn added(&self, ctx: &Watcher) -> WatchResult<DevicePathStream<Child::Parent, Child>> {
        Child::added_in(self.parent, ctx)
    }

    /// Returns a stream of [`DevicePath`]s that are changed and children of
    /// this collection's parent path.
    pub fn changed(&self, ctx: &Watcher) -> WatchResult<DevicePathStream<Child::Parent, Child>> {
        Child::changed_in(self.parent, ctx)
    }

    /// Returns a stream of [`DevicePath`]s that are removed and children of
    /// this collection's parent path.
    pub fn removed(&self, ctx: &Watcher) -> WatchResult<DevicePathStream<Child::Parent, Child>> {
        Child::removed_in(self.parent, ctx)
    }
}

/// An open device from the sysfs.
///
/// This contains a live file handle for the device. If the device disappears
/// while this is open, then any property methods and child accessors will
/// generally return an [`Error::Io`] containing
/// [`std::io::ErrorKind::NotFound`].
pub trait Device: Sealed + Sized {
    type Path: DevicePath;

    fn from_fd(dfd: OwnedFd, path: Self::Path) -> Self;

    /// Returns the [`DevicePath`] that points to this device.
    fn path(&self) -> &Self::Path;

    /// Opens the device of this type at the given path.
    fn open(path: Self::Path) -> Result<Self> {
        let mut sys = Utf8PathBuf::from("/sys/class/typec");
        path.build_syspath(&mut sys);
        let dfd = openat(
            CWD,
            sys.as_str(),
            OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            Mode::empty(),
        )?;
        Ok(Self::from_fd(dfd, path))
    }
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

/// An unopened [`Device`] within a parent.
///
/// Use [`Self::open`] to actually open it and obtain the [`Device`].
#[derive(Debug)]
pub struct DeviceEntry<'fd, T: Device> {
    parent_dfd: BorrowedFd<'fd>,
    path: T::Path,
}

impl<T: Device> DeviceEntry<'_, T> {
    pub fn path(&self) -> &T::Path {
        &self.path
    }

    /// Opens the [`Device`] that this entry points to.
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

/// A collection of child [`Device`]s of a single type beneath a parent.
pub struct DeviceCollection<'fd, Child: Device> {
    dfd: MaybeOwnedFd<'fd>,
    parent: <Child::Path as DevicePath>::Parent,
    phantom: PhantomData<Child>,
}

impl<Child: Device> DeviceCollection<'_, Child> {
    /// Returns an iterator of [`DeviceEntry`]s in this collection.
    pub fn iter(&self) -> Result<DeviceIter<'_, Child>> {
        Ok(DeviceIter {
            dfd: self.dfd.as_fd(),
            dir: Dir::read_from(&self.dfd)?,
            parent: self.parent,
            phantom: PhantomData,
        })
    }

    /// Returns an iterator of open [`Device`]s in this collection.
    pub fn iter_opened(&self) -> Result<impl Iterator<Item = Result<Child>> + '_> {
        let iter = self.iter()?;
        Ok(iter.map(|x| x.and_then(|x| x.open())))
    }

    /// Returns an list of [`DeviceEntry`]s in this collection.
    pub fn list(&self) -> Result<Vec<DeviceEntry<'_, Child>>> {
        self.iter().and_then(|x| x.collect())
    }

    /// Returns an list of open [`Device`]s in this collection.
    pub fn list_opened(&self) -> Result<Vec<Child>> {
        self.iter_opened().and_then(|x| x.collect())
    }
}

impl<Child: Device> DeviceCollection<'_, Child>
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

/// An iterator over child [`Device`]s.
///
/// Obtained via [`DeviceCollection.iter`].
pub struct DeviceIter<'fd, Child: Device> {
    dfd: BorrowedFd<'fd>,
    dir: Dir,
    parent: <Child::Path as DevicePath>::Parent,
    phantom: PhantomData<Child>,
}

impl<'fd, Child: Device> Iterator for DeviceIter<'fd, Child> {
    type Item = Result<DeviceEntry<'fd, Child>>;

    fn next(&mut self) -> Option<Self::Item> {
        for entry in &mut self.dir {
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

/// A path to a [`Port`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PortPath {
    /// The port number, as used in `/sys/class/typec/port[port]`.
    pub port: u32,
}

impl PortPath {
    pub fn collection() -> DevicePathCollection<PortPath> {
        DevicePathCollection { parent: NoParent }
    }

    /// Returns the path for an attached cable.
    pub fn cable(&self) -> CablePath {
        CablePath { port: self.port }
    }

    /// Returns the path for an attached partner device.
    pub fn partner(&self) -> PartnerPath {
        PartnerPath { port: self.port }
    }

    device_path_child_collection_getter!(
        alt_modes,
        AltModePath<PortPath>,
        doc("Returns a path collection for this port's alternate modes."),
    );
}

impl_sealed!(PortPath);

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

/// A USB type-C port on the system.
#[derive(Debug)]
pub struct Port {
    dfd: OwnedFd,
    path: PortPath,
}

impl_sealed!(Port);
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
        doc("The port's currently selected role in data transmission."),
    );
    property!(
        port_type,
        rw(RoleSelection<PortType>, PortType),
        with(PropertyRoleSelection::<PortType>),
        doc("The port's currently selected type."),
    );
    property!(
        power_role,
        rw(RoleSelection<PowerRole>, PowerRole),
        with(PropertyRoleSelection::<PowerRole>),
        doc("The port's currently selected role in power transmission."),
    );
    property!(
        preferred_role,
        ro(Option<PowerRole>),
        with(PropertyPreferredRole),
        doc("If this port is dual-role, then its preferred role of the two."),
    );
    property!(power_operation_mode, ro(PowerOperationMode));
    property!(usb_power_delivery_revision, ro(Revision));
    property!(usb_typec_revision, ro(Revision));

    /// Returns a collection of this port's alternate modes.
    pub fn alt_modes(&self) -> DeviceCollection<'_, AltMode<PortPath>> {
        DeviceCollection {
            dfd: MaybeOwnedFd::Borrowed(self.dfd.as_fd()),
            parent: self.path,
            phantom: PhantomData,
        }
    }

    /// Returns the entry for this port's connected cable.
    pub fn cable(&self) -> DeviceEntry<'_, Cable> {
        DeviceEntry {
            parent_dfd: self.dfd.as_fd(),
            path: self.path.cable(),
        }
    }

    /// Returns the entry for this port's connected partner.
    pub fn partner(&self) -> DeviceEntry<'_, Partner> {
        DeviceEntry {
            parent_dfd: self.dfd.as_fd(),
            path: self.path.partner(),
        }
    }
}

/// A path to a [`Partner`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PartnerPath {
    /// The number of the port this partner is connected to.
    pub port: u32,
}

impl PartnerPath {
    device_path_child_collection_getter!(
        alt_modes,
        AltModePath<PartnerPath>,
        doc("Returns a path collection for this partner's alternate modes."),
    );
    device_path_child_collection_getter!(
        pds,
        PowerDeliveryPath,
        doc("Returns a path collection for this partner's power delivery devices."),
    );
}

impl_sealed!(PartnerPath);

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

/// A connected partner device.
#[derive(Debug)]
pub struct Partner {
    dfd: OwnedFd,
    path: PartnerPath,
}

impl_sealed!(Partner);
impl_device!(Partner, path(PartnerPath));

impl Partner {
    // TODO: type

    property!(usb_power_delivery_revision, ro(Revision));

    /// Returns a handle to the identity information for this partner.
    pub fn identity(&self) -> IdentityPartner<'_> {
        IdentityPartner {
            dfd: self.dfd.as_fd(),
        }
    }

    /// Returns a collection of this partner's alternate modes.
    pub fn alt_modes(&self) -> DeviceCollection<'_, AltMode<PartnerPath>> {
        DeviceCollection {
            dfd: MaybeOwnedFd::Borrowed(self.dfd.as_fd()),
            parent: self.path,
            phantom: PhantomData,
        }
    }

    /// Returns a collection of this partner's power delivery devices.
    pub fn pds(&self) -> DeviceCollection<'_, PowerDelivery> {
        DeviceCollection {
            dfd: MaybeOwnedFd::Borrowed(self.dfd.as_fd()),
            parent: self.path,
            phantom: PhantomData,
        }
    }
}

/// A path to a [`Cable`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CablePath {
    /// The number of the port this cable is connected to.
    pub port: u32,
}

impl CablePath {
    device_path_child_collection_getter!(
        plugs,
        PlugPath,
        doc("Returns a path collection for this cable's plugs."),
    );
}

impl_sealed!(CablePath);

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

/// A connected cable.
#[derive(Debug)]
pub struct Cable {
    dfd: OwnedFd,
    path: CablePath,
}

impl_sealed!(Cable);
impl_device!(Cable, path(CablePath));

impl Cable {
    /// Returns a handle to the identity information for this cable.
    pub fn identity(&self) -> IdentityCable<'_> {
        IdentityCable {
            dfd: self.dfd.as_fd(),
        }
    }

    /// Returns a collection of this cable's plugs.
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

/// A path to a [`Plug`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PlugPath {
    /// The number of the port this plug's cable is connected to.
    pub port: u32,
    /// The number of this plug in the parent cable's collection.
    pub plug: u32,
}

impl PlugPath {
    device_path_child_collection_getter!(
        alt_modes,
        AltModePath<PlugPath>,
        doc("Returns a path collection for this plug's alternate modes."),
    );
}

impl_sealed!(PlugPath);

impl DevicePath for PlugPath {
    type Parent = CablePath;

    fn parse_basename(s: &str, parent: Self::Parent) -> Option<Self> {
        let (a, b) = s.split_once('-')?;
        let parent =
            <Self::Parent as DevicePath>::Parent::parse_basename(a, parent.parent().parent())?;

        let b = b.strip_prefix("plug")?;
        let plug = u32::from_str(b).ok()?;

        Some(Self {
            port: parent.port,
            plug,
        })
    }

    fn build_basename(&self, s: &mut String) {
        self.parent().parent().build_basename(s);
        write!(s, "-plug{}", self.plug).unwrap();
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
            plug: index,
        }
    }
}

impl_device_path_watchable_from_parent!(PlugPath, plug);

/// A [`Cable`]'s plug.
#[derive(Debug)]
pub struct Plug {
    dfd: OwnedFd,
    path: PlugPath,
}

impl_sealed!(Plug);
impl_device!(Plug, path(PlugPath));

impl Plug {
    /// Returns a collection of this plug's alternate modes.
    pub fn alt_modes(&self) -> DeviceCollection<'_, AltMode<PlugPath>> {
        DeviceCollection {
            dfd: MaybeOwnedFd::Borrowed(self.dfd.as_fd()),
            parent: self.path,
            phantom: PhantomData,
        }
    }
}

/// A path to an [`AltMode`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AltModePath<Parent: DevicePath> {
    /// The path of this alternate mode's parent device.
    pub parent: Parent,
    /// The index of this alternate mode in the parent's collection.
    pub index: u32,
}

impl_sealed!(AltModePath<Parent>, forall(<Parent: DevicePath>));

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

impl_device_path_watchable_from_parent!(AltModePath<PartnerPath>, partner_alt_mode);
impl_device_path_watchable_from_parent!(AltModePath<PlugPath>, plug_alt_mode);

/// An alternate mode of some parent device.
#[derive(Debug)]
pub struct AltMode<Parent: DevicePath> {
    dfd: OwnedFd,
    path: AltModePath<Parent>,
}

impl_sealed!(AltMode<Parent>, forall(<Parent: DevicePath>));
impl_device!(AltMode<Parent>, forall(<Parent: DevicePath>), path(AltModePath<Parent>));

impl<Parent: DevicePath> AltMode<Parent> {
    property!(
        active,
        rw(bool),
        with(PropertyBoolYesNo),
        doc("Is this alternate mode currently active?"),
    );
    property!(mode, ro(u32));
    property!(
        svid,
        ro(u16),
        with(PropertyHexU16),
        doc("A Standard or Vendor ID used to identity this mode"),
    );
    property!(vdo, ro(u32), with(PropertyHexPrefixedU32));
}

impl AltMode<PortPath> {
    property!(
        supported_roles,
        ro(SupportedRoles),
        doc("The roles supported by this alternate mode"),
    );
}

/// A handle to a [`Partner`]'s identity information.
#[derive(Debug)]
pub struct IdentityPartner<'fd> {
    dfd: BorrowedFd<'fd>,
}

impl IdentityPartner<'_> {
    property!(
        id_header,
        ro(VdoIdHeaderPartner),
        from(subdir("identity")),
        doc("The identity header.

If this property is not available yet, then calling [`PropertyReadable::get`]
will return [`Error::IdentityUnavailable`]."),
    );
    property!(
        cert_stat,
        ro(VdoCertStat),
        from(subdir("identity")),
        doc("The XID from a USB-IF certified device.

If this property is not available, either because it has not been determined yet
or the device is not USB-IF certified, then calling [`PropertyReadable::get`]
will return [`Error::IdentityUnavailable`]."),
    );
    property!(
        product,
        ro(VdoProduct),
        from(subdir("identity")),
        doc("The product IDs.

If this property is not available yet, then calling [`PropertyReadable::get`]
will return [`Error::IdentityUnavailable`]."),
    );

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

/// A handle to a [`Cable`]'s identity information.
#[derive(Debug)]
pub struct IdentityCable<'fd> {
    dfd: BorrowedFd<'fd>,
}

impl IdentityCable<'_> {
    property!(
        id_header,
        ro(VdoIdHeaderCable),
        from(subdir("identity")),
        doc("The identity header.

If this property is not available yet, then calling [`PropertyReadable::get`]
will return [`Error::IdentityUnavailable`]."),
    );
    property!(
        cert_stat,
        ro(VdoCertStat),
        from(subdir("identity")),
        doc("The XID from a USB-IF certified device.

If this property is not available, either because it has not been determined yet
or the device is not USB-IF certified, then calling [`PropertyReadable::get`]
will return [`Error::IdentityUnavailable`]."),
    );
    property!(
        product,
        ro(VdoProduct),
        from(subdir("identity")),
        doc("The product IDs.

If this property is not available yet, then calling [`PropertyReadable::get`]
will return [`Error::IdentityUnavailable`]."),
    );
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

/// A path to a [`PowerDelivery`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PowerDeliveryPath {
    /// The number of the port this device is connected to.
    pub port: u32,
    /// The index of this device in the partner's collection.
    pub pd: u32,
}

impl PowerDeliveryPath {
    /// Returns a path for the capabilities as a source of power.
    pub fn source_capabilities(&self) -> CapabilitiesPath {
        CapabilitiesPath {
            port: self.port,
            pd: self.pd,
            role: PowerRole::Source,
        }
    }

    /// Returns a path for the capabilities as a sink of power.
    pub fn sink_capabilities(&self) -> CapabilitiesPath {
        CapabilitiesPath {
            port: self.port,
            pd: self.pd,
            role: PowerRole::Sink,
        }
    }
}

impl_sealed!(PowerDeliveryPath);

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

/// A device containing source and sink capability information.
#[derive(Debug)]
pub struct PowerDelivery {
    dfd: OwnedFd,
    path: PowerDeliveryPath,
}

impl_sealed!(PowerDelivery);
impl_device!(PowerDelivery, path(PowerDeliveryPath));

impl PowerDelivery {
    /// Returns the entry for the capabilities as a source of power.
    pub fn source_capabilities(&self) -> DeviceEntry<'_, SourceCapabilities> {
        DeviceEntry {
            parent_dfd: self.dfd.as_fd(),
            path: self.path.source_capabilities(),
        }
    }

    /// Returns the entry for the capabilities as a sink of power.
    pub fn sink_capabilities(&self) -> DeviceEntry<'_, SinkCapabilities> {
        DeviceEntry {
            parent_dfd: self.dfd.as_fd(),
            path: self.path.sink_capabilities(),
        }
    }
}

/// A path to a [`SourceCapabilities`] or [`SinkCapabilities`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CapabilitiesPath {
    /// The number of the port this device is connected to.
    pub port: u32,
    /// The index of this device's parent in the partner's collection.
    pub pd: u32,
    /// Is this a source or a sink?
    pub role: PowerRole,
}

impl_sealed!(CapabilitiesPath);

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

/// A device containing the capabilities of a source of power.
#[derive(Debug)]
pub struct SourceCapabilities {
    dfd: OwnedFd,
    path: CapabilitiesPath,
}

impl_sealed!(SourceCapabilities);
impl_device!(SourceCapabilities, path(CapabilitiesPath));

impl SourceCapabilities {
    /// Returns a collection of source PDOs.
    pub fn pdos(&self) -> DeviceCollection<'_, SourcePdo> {
        DeviceCollection {
            dfd: MaybeOwnedFd::Borrowed(self.dfd.as_fd()),
            parent: self.path,
            phantom: PhantomData,
        }
    }
}

// XXX: copy-pasted struct, but making this generic like AltMode is hard
/// A device containing the capabilities of a sink of power.
#[derive(Debug)]
pub struct SinkCapabilities {
    dfd: OwnedFd,
    path: CapabilitiesPath,
}

impl_sealed!(SinkCapabilities);
impl_device!(SinkCapabilities, path(CapabilitiesPath));

impl SinkCapabilities {
    /// Returns a collection of sink PDOs.
    pub fn pdos(&self) -> DeviceCollection<'_, SinkPdo> {
        DeviceCollection {
            dfd: MaybeOwnedFd::Borrowed(self.dfd.as_fd()),
            parent: self.path,
            phantom: PhantomData,
        }
    }
}

/// The type of a power supply, as exposed in a PDO.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumString, strum::Display)]
#[strum(serialize_all = "snake_case")]
pub enum SupplyKind {
    /// A fixed-voltage supply.
    FixedSupply,
    /// A connected battery.
    Battery,
    /// A supply with a variable voltage within a given range.
    VariableSupply,
    /// A supply whose voltage can be changed dynamically.
    ProgrammableSupply,
}

/// A path to a [`SourcePdo`] or [`SinkPdo`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PdoPath {
    /// The number of the port this device is connected to.
    pub port: u32,
    /// The index of this pdo's [`PowerDeliveryPath`] the partner's collection.
    pub pd: u32,

    /// Is this a source or a sink?
    pub role: PowerRole,
    /// The index of this device in the parent's collection.
    pub index: u32,
    /// The type of power supply.
    pub supply: SupplyKind,
}

impl_sealed!(PdoPath);

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

/// A Power Data Object describing a single source power supply.
#[derive(Debug)]
pub enum SourcePdo {
    FixedSupply(SourcePdoFixedSupply),
    Battery(SourcePdoBattery),
    VariableSupply(SourcePdoVariableSupply),
    ProgrammableSupply(SourcePdoProgrammableSupply),
}

impl_sealed!(SourcePdo);

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

/// A Power Data Object describing a single sink power supply.
#[derive(Debug)]
pub enum SinkPdo {
    FixedSupply(SinkPdoFixedSupply),
    Battery(SinkPdoBattery),
    VariableSupply(SinkPdoVariableSupply),
    ProgrammableSupply(SinkPdoProgrammableSupply),
}

impl_sealed!(SinkPdo);

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

/// A fixed-voltage source power supply.
#[derive(Debug)]
pub struct SourcePdoFixedSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_sealed!(SourcePdoFixedSupply);
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

/// A fixed-voltage power sink.
#[derive(Debug)]
pub struct SinkPdoFixedSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_sealed!(SinkPdoFixedSupply);
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

/// A battery source power supply.
#[derive(Debug)]
pub struct SourcePdoBattery {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_sealed!(SourcePdoBattery);
impl_device!(SourcePdoBattery, path(PdoPath));

impl SourcePdoBattery {
    property!(maximum_voltage, ro(Millivolts));
    property!(minimum_voltage, ro(Millivolts));
    property!(maximum_power, ro(Milliwatts));
}

/// A battery power sink.
#[derive(Debug)]
pub struct SinkPdoBattery {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_sealed!(SinkPdoBattery);
impl_device!(SinkPdoBattery, path(PdoPath));

impl SinkPdoBattery {
    property!(maximum_voltage, ro(Millivolts));
    property!(minimum_voltage, ro(Millivolts));
    property!(operational_power, ro(Milliwatts));
}

/// A source power supply with a variable voltage within a given range.
#[derive(Debug)]
pub struct SourcePdoVariableSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_sealed!(SourcePdoVariableSupply);
impl_device!(SourcePdoVariableSupply, path(PdoPath));

impl SourcePdoVariableSupply {
    property!(maximum_voltage, ro(Millivolts));
    property!(minimum_voltage, ro(Millivolts));
    property!(maximum_current, ro(Milliamps));
}

/// A power sink with a variable voltage within a given range.
#[derive(Debug)]
pub struct SinkPdoVariableSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_sealed!(SinkPdoVariableSupply);
impl_device!(SinkPdoVariableSupply, path(PdoPath));

impl SinkPdoVariableSupply {
    property!(maximum_voltage, ro(Millivolts));
    property!(minimum_voltage, ro(Millivolts));
    property!(operational_current, ro(Milliamps));
}

/// A source power supply whose voltage can be changed dynamically.
#[derive(Debug)]
pub struct SourcePdoProgrammableSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_sealed!(SourcePdoProgrammableSupply);
impl_device!(SourcePdoProgrammableSupply, path(PdoPath));

impl SourcePdoProgrammableSupply {
    property!(power_limited, ro(bool), with(PropertyBoolIntegral));
    property!(maximum_voltage, ro(Millivolts));
    property!(minimum_voltage, ro(Millivolts));
    property!(maximum_current, ro(Milliamps));
}

/// A power sink whose voltage can be changed dynamically.
#[derive(Debug)]
pub struct SinkPdoProgrammableSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_sealed!(SinkPdoProgrammableSupply);
impl_device!(SinkPdoProgrammableSupply, path(PdoPath));

impl SinkPdoProgrammableSupply {
    property!(maximum_voltage, ro(Millivolts));
    property!(minimum_voltage, ro(Millivolts));
    property!(maximum_current, ro(Milliamps));
}
