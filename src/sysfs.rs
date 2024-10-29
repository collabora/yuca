use std::{
    fmt,
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

struct ChildIter<
    Params,
    Child,
    Parse: Fn(&str) -> Option<Params>,
    Create: Fn(OwnedFd, Params) -> Child,
> {
    dfd: OwnedFd,
    dir: Dir,
    child_oflags: OFlags,
    parse: Parse,
    create: Create,
}

impl<Params, Child, Parse: Fn(&str) -> Option<Params>, Create: Fn(OwnedFd, Params) -> Child>
    Iterator for ChildIter<Params, Child, Parse, Create>
{
    type Item = Result<Child>;

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

            let Some(params) = (self.parse)(name) else {
                continue;
            };

            return Some(
                openat(&self.dfd, name, self.child_oflags, Mode::empty())
                    .map(|fd| (self.create)(fd, params))
                    .map_err(|e| e.into()),
            );
        }

        None
    }
}

pub struct Port {
    dfd: OwnedFd,
    port: u32,
}

impl Port {
    pub fn list() -> Result<impl Iterator<Item = Result<Port>>> {
        let dfd = openat(
            CWD,
            "/sys/class/typec",
            OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            Mode::empty(),
        )?;

        Ok(ChildIter {
            dir: Dir::read_from(&dfd)?,
            dfd,
            child_oflags: OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            parse: |s| {
                let s = s.strip_prefix("port")?;
                let port = u32::from_str(s).ok()?;
                Some(port)
            },
            create: |dfd, port| Port { dfd, port },
        })
    }

    pub fn port(&self) -> u32 {
        self.port
    }

    pub fn cable(&self) -> Result<Cable> {
        let port = self.port;
        let dfd = openat(
            &self.dfd,
            format!("port{port}-cable"),
            OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            Mode::empty(),
        )?;
        Ok(Cable { dfd, port })
    }

    pub fn partner(&self) -> Result<Partner> {
        let port = self.port;
        let dfd = openat(
            &self.dfd,
            format!("port{port}-partner"),
            OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            Mode::empty(),
        )?;
        Ok(Partner { dfd, port })
    }

    pub fn usb_power_delivery(&self) -> Result<UsbPowerDelivery> {
        let dfd = openat(
            &self.dfd,
            "usb_power_delivery",
            OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            Mode::empty(),
        )?;
        Ok(UsbPowerDelivery { dfd })
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

    pub fn alt_modes(&self) -> Result<impl Iterator<Item = Result<AltMode>>> {
        // TODO: dedup
        // TODO: remove unneeded clone?
        let dfd = self.dfd.try_clone()?;

        Ok(ChildIter {
            dir: Dir::read_from(&dfd)?,
            dfd,
            child_oflags: OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            parse: |s| {
                let (_, index) = s.split_once('.')?;
                let index = u32::from_str(index).ok()?;
                Some(index)
            },
            create: |dfd, index| AltMode { dfd, index },
        })
    }

    pub fn plugs(&self) -> Result<impl Iterator<Item = Result<Plug>>> {
        let port = self.port;
        // TODO: remove unneeded clone?
        let dfd = self.dfd.try_clone()?;

        Ok(ChildIter {
            dir: Dir::read_from(&dfd)?,
            dfd,
            child_oflags: OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            parse: |s| {
                let (_, index) = s.split_once('.')?;
                let index = index.strip_prefix("plug")?;
                let index = u32::from_str(index).ok()?;
                Some(index)
            },
            create: move |dfd, index| Plug { dfd, port, index },
        })
    }
}

pub struct Partner {
    dfd: OwnedFd,
    port: u32,
}

impl Partner {
    pub fn port(&self) -> u32 {
        self.port
    }

    // TODO: type
    property_ro!(usb_power_delivery_revision, Revision);

    pub fn identity(&self) -> IdentityPartner<'_> {
        IdentityPartner {
            dfd: self.dfd.as_fd(),
        }
    }

    pub fn alt_modes(&self) -> Result<impl Iterator<Item = Result<AltMode>>> {
        // TODO: dedup
        // TODO: remove unneeded clone?
        let dfd = self.dfd.try_clone()?;

        Ok(ChildIter {
            dir: Dir::read_from(&dfd)?,
            dfd,
            child_oflags: OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            parse: |s| {
                let (_, index) = s.split_once('.')?;
                let index = u32::from_str(index).ok()?;
                Some(index)
            },
            create: |dfd, index| AltMode { dfd, index },
        })
    }

    pub fn usb_power_delivery(&self) -> Result<UsbPowerDelivery> {
        let dfd = openat(
            &self.dfd,
            "usb_power_delivery",
            OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            Mode::empty(),
        )?;
        Ok(UsbPowerDelivery { dfd })
    }
}

pub struct Cable {
    dfd: OwnedFd,
    port: u32,
}

impl Cable {
    pub fn port(&self) -> u32 {
        self.port
    }

    pub fn identity(&self) -> IdentityCable<'_> {
        IdentityCable {
            dfd: self.dfd.as_fd(),
        }
    }

    property_ro!(cable_type, CableType, from("type"));
    property_ro!(plug_type, PlugType);
    property_ro!(usb_power_delivery_revision, Revision);
}

pub struct Plug {
    dfd: OwnedFd,
    port: u32,
    index: u32,
}

impl Plug {
    pub fn port(&self) -> u32 {
        self.port
    }

    pub fn index(&self) -> u32 {
        self.index
    }

    pub fn alt_modes(&self) -> Result<impl Iterator<Item = Result<AltMode>>> {
        // TODO: remove unneeded clone?
        let dfd = self.dfd.try_clone()?;

        Ok(ChildIter {
            dir: Dir::read_from(&dfd)?,
            dfd,
            child_oflags: OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            parse: |s| {
                let (_, index) = s.split_once('.')?;
                let index = u32::from_str(index).ok()?;
                Some(index)
            },
            create: |dfd, index| AltMode { dfd, index },
        })
    }
}

pub struct AltMode {
    dfd: OwnedFd,
    index: u32,
}

impl AltMode {
    pub fn index(&self) -> u32 {
        self.index
    }

    // do we need 'mode'? is it different from the already-present index?

    property_rw!(active, bool, with(PropertyBoolYesNo));
    property_ro!(supported_roles, SupportedRoles);
    property_ro!(svid, u16, with(PropertyHexU16));
    property_ro!(vdo, u32, with(PropertyHexU32));
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString)]
#[strum(serialize_all = "snake_case")]
enum SupplyName {
    FixedSupply,
    Battery,
    VariableSupply,
    ProgrammableSupply,
}

pub struct UsbPowerDelivery {
    dfd: OwnedFd,
}

impl UsbPowerDelivery {
    fn capabilities<Capabilities>(
        &self,
        name: &str,
        create: impl Fn(OwnedFd, (u32, SupplyName)) -> Capabilities,
    ) -> Result<impl Iterator<Item = Result<Capabilities>>> {
        let dfd = openat(
            &self.dfd,
            name,
            OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            Mode::empty(),
        )?;

        Ok(ChildIter {
            dir: Dir::read_from(&dfd)?,
            dfd,
            child_oflags: OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            parse: |s| {
                let (index, supply) = s.split_once(':')?;
                let index = u32::from_str(index).ok()?;
                let supply = SupplyName::from_str(supply).ok()?;
                Some((index, supply))
            },
            create,
        })
    }

    pub fn source_capabilities(&self) -> Result<impl Iterator<Item = Result<SourceCapabilities>>> {
        self.capabilities("source-capabilities", |dfd, (index, supply)| match supply {
            SupplyName::FixedSupply => {
                SourceCapabilities::FixedSupply(SourceCapabilitiesFixedSupply { dfd, index })
            }
            SupplyName::Battery => {
                SourceCapabilities::Battery(SourceCapabilitiesBattery { dfd, index })
            }
            SupplyName::VariableSupply => {
                SourceCapabilities::VariableSupply(SourceCapabilitiesVariableSupply { dfd, index })
            }
            SupplyName::ProgrammableSupply => {
                SourceCapabilities::ProgrammableSupply(SourceCapabilitiesProgrammableSupply {
                    dfd,
                    index,
                })
            }
        })
    }

    pub fn sink_capabilities(&self) -> Result<impl Iterator<Item = Result<SinkCapabilities>>> {
        self.capabilities("sink-capabilities", |dfd, (index, supply)| match supply {
            SupplyName::FixedSupply => {
                SinkCapabilities::FixedSupply(SinkCapabilitiesFixedSupply { dfd, index })
            }
            SupplyName::Battery => {
                SinkCapabilities::Battery(SinkCapabilitiesBattery { dfd, index })
            }
            SupplyName::VariableSupply => {
                SinkCapabilities::VariableSupply(SinkCapabilitiesVariableSupply { dfd, index })
            }
            SupplyName::ProgrammableSupply => {
                SinkCapabilities::ProgrammableSupply(SinkCapabilitiesProgrammableSupply {
                    dfd,
                    index,
                })
            }
        })
    }
}

pub enum SourceCapabilities {
    FixedSupply(SourceCapabilitiesFixedSupply),
    Battery(SourceCapabilitiesBattery),
    VariableSupply(SourceCapabilitiesVariableSupply),
    ProgrammableSupply(SourceCapabilitiesProgrammableSupply),
}

pub enum SinkCapabilities {
    FixedSupply(SinkCapabilitiesFixedSupply),
    Battery(SinkCapabilitiesBattery),
    VariableSupply(SinkCapabilitiesVariableSupply),
    ProgrammableSupply(SinkCapabilitiesProgrammableSupply),
}

pub struct SourceCapabilitiesFixedSupply {
    dfd: OwnedFd,
    index: u32,
}

impl SourceCapabilitiesFixedSupply {
    pub fn index(&self) -> u32 {
        self.index
    }

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

pub struct SinkCapabilitiesFixedSupply {
    dfd: OwnedFd,
    index: u32,
}

impl SinkCapabilitiesFixedSupply {
    pub fn index(&self) -> u32 {
        self.index
    }

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

pub struct SourceCapabilitiesBattery {
    dfd: OwnedFd,
    index: u32,
}

impl SourceCapabilitiesBattery {
    pub fn index(&self) -> u32 {
        self.index
    }

    property_ro!(maximum_voltage, Millivolts);
    property_ro!(minimum_voltage, Millivolts);
    property_ro!(maximum_power, Milliwatts);
}

pub struct SinkCapabilitiesBattery {
    dfd: OwnedFd,
    index: u32,
}

impl SinkCapabilitiesBattery {
    pub fn index(&self) -> u32 {
        self.index
    }

    property_ro!(maximum_voltage, Millivolts);
    property_ro!(minimum_voltage, Millivolts);
    property_ro!(operational_power, Milliwatts);
}

pub struct SourceCapabilitiesVariableSupply {
    dfd: OwnedFd,
    index: u32,
}

impl SourceCapabilitiesVariableSupply {
    pub fn index(&self) -> u32 {
        self.index
    }

    property_ro!(maximum_voltage, Millivolts);
    property_ro!(minimum_voltage, Millivolts);
    property_ro!(maximum_current, Milliamps);
}

pub struct SinkCapabilitiesVariableSupply {
    dfd: OwnedFd,
    index: u32,
}

impl SinkCapabilitiesVariableSupply {
    pub fn index(&self) -> u32 {
        self.index
    }

    property_ro!(maximum_voltage, Millivolts);
    property_ro!(minimum_voltage, Millivolts);
    property_ro!(operational_current, Milliamps);
}

pub struct SourceCapabilitiesProgrammableSupply {
    dfd: OwnedFd,
    index: u32,
}

impl SourceCapabilitiesProgrammableSupply {
    pub fn index(&self) -> u32 {
        self.index
    }

    property_ro!(power_limited, bool, with(PropertyBoolIntegral));
    property_ro!(maximum_voltage, Millivolts);
    property_ro!(minimum_voltage, Millivolts);
    property_ro!(maximum_current, Milliamps);
}

pub struct SinkCapabilitiesProgrammableSupply {
    dfd: OwnedFd,
    index: u32,
}

impl SinkCapabilitiesProgrammableSupply {
    pub fn index(&self) -> u32 {
        self.index
    }

    property_ro!(maximum_voltage, Millivolts);
    property_ro!(minimum_voltage, Millivolts);
    property_ro!(maximum_current, Milliamps);
}
