use std::{
    fmt::{self, Write as FmtWrite},
    fs::File,
    io::{Read, Write},
    marker::PhantomData,
    str::FromStr,
};

use rustix::{
    fd::{AsFd, BorrowedFd, OwnedFd},
    fs::{openat, Dir, Mode, OFlags, CWD},
    path::Arg,
};
use strum::EnumString;

use crate::{types::*, Error, Result};

trait PropertyReader {
    type Read;

    fn read(s: &str) -> Result<Self::Read>;
}

trait PropertyWriter: PropertyReader {
    type Write;

    fn write(dest: impl Write, value: &Self::Write) -> Result<()>;
}

pub trait PropertyReadable {
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

impl<R: FromStr> PropertyReader for PropertyParse<R> {
    type Read = R;

    fn read(s: &str) -> Result<R> {
        s.parse().or(Err(Error::Parse))
    }
}

struct PropertyParseDisplay<R: FromStr, W: fmt::Display> {
    _phantom: PhantomData<(R, W)>,
}

impl<R: FromStr, W: fmt::Display> PropertyReader for PropertyParseDisplay<R, W> {
    type Read = R;

    fn read(s: &str) -> Result<R> {
        PropertyParse::<R>::read(s)
    }
}

impl<R: FromStr, W: fmt::Display> PropertyWriter for PropertyParseDisplay<R, W> {
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

struct PropertyHexU32;

impl PropertyReader for PropertyHexU32 {
    type Read = u32;

    fn read(s: &str) -> Result<Self::Read> {
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

macro_rules! property_ro {
    ($name:ident, $read:ty $(, from($from:expr))?) => {
        property_ro!($name, $read, with(PropertyParse::<$read>) $(, from($from))?);
    };
    ($name:ident, $read:ty, with($impl:ty) $(, from($from:expr))?) => {
        pub fn $name(&self) -> impl PropertyReadable<Read = $read> + '_ {
            PropertyImpl::<'_, $impl>::new(
                self.dfd.as_fd(),
                ($($from,)? stringify!($name),).0,
            )
        }
    };
}

macro_rules! property_rw {
    ($name:ident, $rw:ty, with($impl:ty) $(, from($from:expr))?) => {
        property_rw!($name, $rw, $rw, with($impl) $(, from($from))?);
    };
    ($name:ident, $rw:ty, with($impl:ty) $(, from($from:expr))?) => {
        property_rw!($name, $rw, $rw, with($impl) $(, from($from))?);
    };
    ($name:ident, $read:ty, $write:ty, with($impl:ty) $(, from($from:expr))?) => {
        pub fn $name(&self) -> impl PropertyWritable<Read = $read, Write = $write> + '_ {
            PropertyImpl::<'_, $impl>::new(
                self.dfd.as_fd(),
                ($($from,)? stringify!($name),).0,
            )
        }
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

pub trait DevicePathParent: fmt::Debug + Copy + Clone + PartialEq + Eq + std::hash::Hash {
    fn build_syspath(&self, s: &mut String);
}

impl<P: DevicePath> DevicePathParent for P {
    fn build_syspath(&self, s: &mut String) {
        self.parent().build_syspath(s);
        if !s.is_empty() {
            s.push('/');
        }
        self.build_basename(s);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NoParent;

impl DevicePathParent for NoParent {
    fn build_syspath(&self, _s: &mut String) {}
}

pub trait DevicePathIndexed: DevicePath {
    type Index;

    fn child_of(parent: Self::Parent, index: Self::Index) -> Self;
}

pub trait Device: Sized {
    type Path: DevicePath;

    fn from_fd(dfd: OwnedFd, path: Self::Path) -> Self;

    fn path(&self) -> &Self::Path;
}

macro_rules! impl_device {
    ($dev:ty, $(forall($($args:tt)*), )? path($path:ty)) => {
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

    property_rw!(
        data_role,
        RoleSelection<DataRole>,
        DataRole,
        with(PropertyRoleSelection::<DataRole>)
    );
    property_rw!(
        port_type,
        RoleSelection<PortType>,
        PortType,
        with(PropertyRoleSelection::<PortType>)
    );
    property_rw!(
        power_role,
        RoleSelection<PowerRole>,
        PowerRole,
        with(PropertyRoleSelection::<PowerRole>)
    );
    property_rw!(
        preferred_role,
        Option<PowerRole>,
        with(PropertyPreferredRole)
    );
    property_ro!(power_operation_mode, PowerOperationMode);
    property_ro!(usb_power_delivery_revision, Revision);
    property_ro!(usb_typec_revision, Revision);

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
            path: CablePath {
                port: self.path.port,
            },
        }
    }

    pub fn partner(&self) -> DeviceEntry<'_, Partner> {
        DeviceEntry {
            parent_dfd: self.dfd.as_fd(),
            path: PartnerPath {
                port: self.path.port,
            },
        }
    }

    pub fn plugs(&self) -> DeviceCollection<'_, Plug> {
        DeviceCollection {
            dfd: MaybeOwnedFd::Borrowed(self.dfd.as_fd()),
            parent: self.path,
            phantom: PhantomData,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PartnerPath {
    pub port: u32,
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

#[derive(Debug)]
pub struct Partner {
    dfd: OwnedFd,
    path: PartnerPath,
}

impl_device!(Partner, path(PartnerPath));

impl Partner {
    // TODO: type
    property_ro!(usb_power_delivery_revision, Revision);

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

    property_ro!(cable_type, CableType, from("type"));
    property_ro!(plug_type, PlugType);
    property_ro!(usb_power_delivery_revision, Revision);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PlugPath {
    pub port: u32,
    pub index: u32,
}

impl DevicePath for PlugPath {
    type Parent = PortPath;

    fn parse_basename(s: &str, parent: Self::Parent) -> Option<Self> {
        let (a, b) = s.split_once('-')?;
        let parent = Self::Parent::parse_basename(a, parent.parent())?;

        let b = b.strip_prefix("plug")?;
        let index = u32::from_str(b).ok()?;

        Some(Self {
            port: parent.port,
            index,
        })
    }

    fn build_basename(&self, s: &mut String) {
        self.parent().build_basename(s);
        write!(s, "plug{}", self.index).unwrap();
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

#[derive(Debug)]
pub struct AltMode<Parent: DevicePath> {
    dfd: OwnedFd,
    path: AltModePath<Parent>,
}

impl_device!(AltMode<Parent>, forall(<Parent: DevicePath>), path(AltModePath<Parent>));

impl<Parent: DevicePath> AltMode<Parent> {
    // do we need 'mode'? is it different from the already-present index?

    property_rw!(active, bool, with(PropertyBoolYesNo));
    property_ro!(supported_roles, SupportedRoles);
    property_ro!(svid, u16, with(PropertyHexU16));
    property_ro!(vdo, u32, with(PropertyHexU32));
}

#[derive(Debug)]
pub struct IdentityPartner<'fd> {
    dfd: BorrowedFd<'fd>,
}

impl IdentityPartner<'_> {
    property_ro!(id_header, VdoIdHeaderPartner);
    property_ro!(cert_stat, VdoCertStat);
    property_ro!(product, VdoProduct);

    // TODO: should these be different types?
    property_ro!(product_type_vdo1, u32);
    property_ro!(product_type_vdo2, u32);
    property_ro!(product_type_vdo3, u32);
}

#[derive(Debug)]
pub struct IdentityCable<'fd> {
    dfd: BorrowedFd<'fd>,
}

impl IdentityCable<'_> {
    property_ro!(id_header, VdoIdHeaderCable);
    property_ro!(cert_stat, VdoCertStat);
    property_ro!(product, VdoProduct);

    // TODO: should these be different types?
    property_ro!(product_type_vdo1, u32);
    property_ro!(product_type_vdo2, u32);
    property_ro!(product_type_vdo3, u32);
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
            path: CapabilitiesPath {
                port: self.path.port,
                pd: self.path.pd,
                role: PowerRole::Source,
            },
        }
    }

    pub fn sink_capabilities(&self) -> DeviceEntry<'_, SinkCapabilities> {
        DeviceEntry {
            parent_dfd: self.dfd.as_fd(),
            path: CapabilitiesPath {
                port: self.path.port,
                pd: self.path.pd,
                role: PowerRole::Source,
            },
        }
    }
}

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
    port: u32,
    pd: u32,

    role: PowerRole,
    index: u32,
    supply: SupplyKind,
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
    FixedSupply(SourceCapabilitiesFixedSupply),
    Battery(SourceCapabilitiesBattery),
    VariableSupply(SourceCapabilitiesVariableSupply),
    ProgrammableSupply(SourceCapabilitiesProgrammableSupply),
}

impl Device for SourcePdo {
    type Path = PdoPath;

    fn from_fd(dfd: OwnedFd, path: Self::Path) -> Self {
        match path.supply {
            SupplyKind::FixedSupply => {
                SourcePdo::FixedSupply(SourceCapabilitiesFixedSupply { dfd, path })
            }
            SupplyKind::Battery => SourcePdo::Battery(SourceCapabilitiesBattery { dfd, path }),
            SupplyKind::VariableSupply => {
                SourcePdo::VariableSupply(SourceCapabilitiesVariableSupply { dfd, path })
            }
            SupplyKind::ProgrammableSupply => {
                SourcePdo::ProgrammableSupply(SourceCapabilitiesProgrammableSupply { dfd, path })
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
    FixedSupply(SinkCapabilitiesFixedSupply),
    Battery(SinkCapabilitiesBattery),
    VariableSupply(SinkCapabilitiesVariableSupply),
    ProgrammableSupply(SinkCapabilitiesProgrammableSupply),
}

impl Device for SinkPdo {
    type Path = PdoPath;

    fn from_fd(dfd: OwnedFd, path: Self::Path) -> Self {
        match path.supply {
            SupplyKind::FixedSupply => {
                SinkPdo::FixedSupply(SinkCapabilitiesFixedSupply { dfd, path })
            }
            SupplyKind::Battery => SinkPdo::Battery(SinkCapabilitiesBattery { dfd, path }),
            SupplyKind::VariableSupply => {
                SinkPdo::VariableSupply(SinkCapabilitiesVariableSupply { dfd, path })
            }
            SupplyKind::ProgrammableSupply => {
                SinkPdo::ProgrammableSupply(SinkCapabilitiesProgrammableSupply { dfd, path })
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
pub struct SourceCapabilitiesFixedSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SourceCapabilitiesFixedSupply, path(PdoPath));

impl SourceCapabilitiesFixedSupply {
    // first item only!
    property_ro!(dual_role_power, bool, with(PropertyBoolIntegral));
    property_ro!(usb_suspend_supported, bool, with(PropertyBoolIntegral));
    property_ro!(unconstrained_power, bool, with(PropertyBoolIntegral));
    property_ro!(usb_communication_capable, bool, with(PropertyBoolIntegral));
    property_ro!(dual_role_data, bool, with(PropertyBoolIntegral));
    property_ro!(
        unchuncked_extended_messages_supported,
        bool,
        with(PropertyBoolIntegral)
    );

    // available on all items
    property_ro!(peak_current, u32);
    property_ro!(voltage, Millivolts);
    property_ro!(maximum_current, Milliamps);
}

#[derive(Debug)]
pub struct SinkCapabilitiesFixedSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SinkCapabilitiesFixedSupply, path(PdoPath));

impl SinkCapabilitiesFixedSupply {
    // first item only!
    property_ro!(dual_role_power, bool, with(PropertyBoolIntegral));
    property_ro!(higher_capability, bool, with(PropertyBoolIntegral));
    property_ro!(unconstrained_power, bool, with(PropertyBoolIntegral));
    property_ro!(usb_communication_capable, bool, with(PropertyBoolIntegral));
    property_ro!(dual_role_data, bool, with(PropertyBoolIntegral));
    property_ro!(
        unchuncked_external_messages,
        bool,
        with(PropertyBoolIntegral)
    );
    property_ro!(fast_role_swap_current, u32);

    // available on all items
    property_ro!(voltage, Millivolts);
    property_ro!(operational_current, Milliamps);
}

#[derive(Debug)]
pub struct SourceCapabilitiesBattery {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SourceCapabilitiesBattery, path(PdoPath));

impl SourceCapabilitiesBattery {
    property_ro!(maximum_voltage, Millivolts);
    property_ro!(minimum_voltage, Millivolts);
    property_ro!(maximum_power, Milliwatts);
}

#[derive(Debug)]
pub struct SinkCapabilitiesBattery {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SinkCapabilitiesBattery, path(PdoPath));

impl SinkCapabilitiesBattery {
    property_ro!(maximum_voltage, Millivolts);
    property_ro!(minimum_voltage, Millivolts);
    property_ro!(operational_power, Milliwatts);
}

#[derive(Debug)]
pub struct SourceCapabilitiesVariableSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SourceCapabilitiesVariableSupply, path(PdoPath));

impl SourceCapabilitiesVariableSupply {
    property_ro!(maximum_voltage, Millivolts);
    property_ro!(minimum_voltage, Millivolts);
    property_ro!(maximum_current, Milliamps);
}

#[derive(Debug)]
pub struct SinkCapabilitiesVariableSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SinkCapabilitiesVariableSupply, path(PdoPath));

impl SinkCapabilitiesVariableSupply {
    property_ro!(maximum_voltage, Millivolts);
    property_ro!(minimum_voltage, Millivolts);
    property_ro!(operational_current, Milliamps);
}

#[derive(Debug)]
pub struct SourceCapabilitiesProgrammableSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SourceCapabilitiesProgrammableSupply, path(PdoPath));

impl SourceCapabilitiesProgrammableSupply {
    property_ro!(power_limited, bool, with(PropertyBoolIntegral));
    property_ro!(maximum_voltage, Millivolts);
    property_ro!(minimum_voltage, Millivolts);
    property_ro!(maximum_current, Milliamps);
}

#[derive(Debug)]
pub struct SinkCapabilitiesProgrammableSupply {
    dfd: OwnedFd,
    path: PdoPath,
}

impl_device!(SinkCapabilitiesProgrammableSupply, path(PdoPath));

impl SinkCapabilitiesProgrammableSupply {
    property_ro!(maximum_voltage, Millivolts);
    property_ro!(minimum_voltage, Millivolts);
    property_ro!(maximum_current, Milliamps);
}
