//! Various types returned from [`sysfs`](crate::sysfs)'s properties.

use std::str::FromStr;

use strum::{Display, EnumString, FromRepr};

use crate::{Error, Result};

/// A single role selected from a potentially-dual-role property.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RoleSelection<R> {
    /// The actual selected role.
    pub role: R,
    /// Whether or not the selected role is one of multiple available.
    pub supports_dual: bool,
}

impl<Role: FromStr> FromStr for RoleSelection<Role>
where
    Error: From<Role::Err>,
{
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let mut supports_dual = false;

        let mut it = s.split(' ');
        while let Some(r) = it.next() {
            if r.starts_with('[') {
                let role = Role::from_str(
                    r.strip_prefix('[')
                        .and_then(|r| r.strip_suffix(']'))
                        .ok_or(Error::Parse)?,
                )?;
                supports_dual = supports_dual || it.next().is_some();
                return Ok(RoleSelection {
                    role,
                    supports_dual,
                });
            }

            supports_dual = true;
        }

        Err(Error::Parse)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString, Display)]
#[strum(serialize_all = "snake_case")]
pub enum PortType {
    Source,
    Sink,
    Dual,
}

/// A port's role in data transmission.
#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString, Display)]
#[strum(serialize_all = "snake_case")]
pub enum DataRole {
    Host,
    Device,
}

/// A port's role in power transmission.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumString, Display)]
#[strum(serialize_all = "snake_case")]
pub enum PowerRole {
    Source,
    Sink,
}

// XXX: should this really be a separate struct?
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SupportedRoles {
    pub source: bool,
    pub sink: bool,
}

impl FromStr for SupportedRoles {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let mut result = Self {
            source: false,
            sink: false,
        };
        for word in s.split(' ') {
            match word {
                "source" => result.source = true,
                "sink" => result.sink = true,
                _ => return Err(Error::Parse),
            }
        }
        Ok(result)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum PowerOperationMode {
    /// Standard USB (non-type-C) power negotiation.
    Default,
    /// Fixed at 1.5A @ 5V.
    #[strum(serialize = "1.5A")]
    TypeC1_5A,
    /// Fixed at 3.0A @ 5V.
    #[strum(serialize = "3A")]
    TypeC3_0A,
    /// USB power delivery support, for negotiating higher power draws.
    #[strum(serialize = "usb_power_delivery")]
    PowerDelivery,
}

/// The type of a USB-C cable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum CableType {
    NotCable, // ???
    Passive,
    Active,
    // A captive cable for a "Vconn Powered USB Device".
    Vpd,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString)]
#[strum(serialize_all = "kebab-case")]
pub enum PlugType {
    Unknown,
    TypeA,
    TypeB,
    TypeC,
    Captive,
}

/// A `[major].[minor]` version.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Revision {
    pub major: u8,
    pub minor: u8,
}

impl FromStr for Revision {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let (major, minor) = s.split_once('.').ok_or(Error::Parse)?;
        Ok(Revision {
            major: major.parse()?,
            minor: minor.parse()?,
        })
    }
}

// XXX: Is it actually possible to read the Not[Ufp/Dfp/Cable] values from
// sysfs? They seem to be distinguished by SOP vs SOP', so I'd imagine the
// kernel should be taking care of distinguishing the values properly? Or maybe
// it's still possible to get there.

// The type of a [`crate::sysfs::Partner`]'s Upstream-Facing Port, as read from
// the [identity header](VdoIdHeader).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProductTypePartnerUfp {
    NotUfp,
    PdUsbHub,
    PdUsbPeripheral,
    Psd,
    Unknown(u8),
}

// The type of a [`crate::sysfs::Partner`]'s Downstream-Facing Port, as read
// from the [identity header](VdoIdHeader).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProductTypePartnerDfp {
    NotDfp,
    PdUsbHub,
    PdUsbHost,
    PowerBrick,
    Unknown(u8),
}

/// The type of a [`crate::sysfs::Cable`], as read from the [identity
/// header](VdoIdHeader).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProductTypeCable {
    // XXX: will this always match the `cable_type` anyway?
    NotCable,
    Passive,
    Active,
    Vpd,
    Unknown(u8),
}

/// The type of a [`crate::sysfs::Partner`]'s connector, as read from the
/// [identity header](VdoIdHeader).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConnectorType {
    Unknown(u8),
    Receptacle,
    Plug,
}

/// ID Header VDO (6.4.4.3.1.1)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VdoIdHeader(pub u32);

impl VdoIdHeader {
    /// Is this product a USB host?
    pub fn usb_host(&self) -> bool {
        self.0 & (1 << 31) != 0
    }

    /// Is this product a USB device?
    pub fn usb_device(&self) -> bool {
        self.0 & (1 << 30) != 0
    }

    /// SOP product type UFP / SOP' product type.
    pub fn product_type_0(&self) -> u8 {
        ((self.0 & (0b111 << 27)) >> 27) as u8
    }

    /// SOP product type UFP / SOP' product type.
    pub fn supports_alt_modes(&self) -> bool {
        self.0 & (1 << 26) != 0
    }

    /// SOP product type DFP / SOP' reserved.
    pub fn product_type_1(&self) -> u8 {
        ((self.0 & (0b111 << 23)) >> 23) as u8
    }

    pub fn connector_type(&self) -> ConnectorType {
        match (self.0 & (0b11 << 21)) >> 21 {
            0b10 => ConnectorType::Receptacle,
            0b01 => ConnectorType::Plug,
            other => ConnectorType::Unknown(other as u8),
        }
    }

    /// Reserved bits 20..16.
    pub fn reserved(&self) -> u8 {
        ((self.0 & (0b11111 << 16)) >> 16) as u8
    }

    pub fn vendor_id(&self) -> u16 {
        (self.0 & 0xFFFF) as u16
    }
}

/// A parsed identity header from a [`crate::sysfs::Partner`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VdoIdHeaderPartner(pub VdoIdHeader);

impl VdoIdHeaderPartner {
    /// Partner UFP product type.
    pub fn product_type_ufp(&self) -> ProductTypePartnerUfp {
        match self.0.product_type_0() {
            0b000 => ProductTypePartnerUfp::NotUfp,
            0b001 => ProductTypePartnerUfp::PdUsbHub,
            0b010 => ProductTypePartnerUfp::PdUsbPeripheral,
            0b011 => ProductTypePartnerUfp::Psd,
            other => ProductTypePartnerUfp::Unknown(other),
        }
    }

    /// Partner DFP product type.
    pub fn product_type_dfp(&self) -> ProductTypePartnerDfp {
        match self.0.product_type_1() {
            0b000 => ProductTypePartnerDfp::NotDfp,
            0b001 => ProductTypePartnerDfp::PdUsbHub,
            0b010 => ProductTypePartnerDfp::PdUsbHost,
            0b011 => ProductTypePartnerDfp::PowerBrick,
            other => ProductTypePartnerDfp::Unknown(other),
        }
    }
}

impl FromStr for VdoIdHeaderPartner {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let s = s.strip_prefix("0x").ok_or(Error::Parse)?;
        let n = u32::from_str_radix(s, 16)?;
        if n == 0 {
            return Err(Error::IdentityUnavailable);
        }

        Ok(VdoIdHeaderPartner(VdoIdHeader(n)))
    }
}

/// A parsed identity header from a [`crate::sysfs::Cable`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VdoIdHeaderCable(pub VdoIdHeader);

impl VdoIdHeaderCable {
    pub fn product_type(&self) -> ProductTypeCable {
        match self.0.product_type_0() {
            0b000 => ProductTypeCable::NotCable,
            0b011 => ProductTypeCable::Passive,
            0b100 => ProductTypeCable::Active,
            0b110 => ProductTypeCable::Vpd,
            other => ProductTypeCable::Unknown(other),
        }
    }

    /// Reserved value from the SOP DFP product type.
    pub fn reserved(&self) -> u8 {
        self.0.product_type_1()
    }
}

impl FromStr for VdoIdHeaderCable {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let s = s.strip_prefix("0x").ok_or(Error::Parse)?;
        let n = u32::from_str_radix(s, 16)?;
        if n == 0 {
            return Err(Error::IdentityUnavailable);
        }

        Ok(VdoIdHeaderCable(VdoIdHeader(n)))
    }
}

/// An XID from a USB-IF-certified device.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VdoCertStat(pub u32);

impl FromStr for VdoCertStat {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let s = s.strip_prefix("0x").ok_or(Error::Parse)?;
        let n = u32::from_str_radix(s, 16)?;
        if n == 0 {
            return Err(Error::IdentityUnavailable);
        }

        Ok(VdoCertStat(n))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VdoProduct {
    /// The device's product ID.
    pub product_id: u16,
    /// The device's release number, as binary-coded decimal.
    pub bcd_device: u16,
}

impl FromStr for VdoProduct {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let s = s.strip_prefix("0x").ok_or(Error::Parse)?;
        let n = u32::from_str_radix(s, 16)?;
        if n == 0 {
            return Err(Error::IdentityUnavailable);
        }

        Ok(VdoProduct {
            product_id: (n >> 16) as u16,
            bcd_device: (n & 0xFFFF) as u16,
        })
    }
}

fn parse_unit_suffixed(s: &str, suffix: &str) -> Result<u32> {
    s.strip_suffix(suffix)
        .and_then(|s| u32::from_str(s).ok())
        .ok_or(Error::Parse)
}

/// The current required by a sink after a fast role swap.
#[derive(Debug, Clone, Copy, PartialEq, Eq, FromRepr)]
#[repr(u32)]
pub enum FastRoleSwapCurrent {
    /// Fast role swaps are not supported.
    NotSupported = 0b00,
    /// Standard USB (non-type-C) power.
    Default = 0b01,
    /// 1.5A @ 5V.
    _1_5A = 0b10,
    /// 3.0A @ 5V.
    _3_0A = 0b11,
}

impl FromStr for FastRoleSwapCurrent {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        s.parse().ok().and_then(Self::from_repr).ok_or(Error::Parse)
    }
}

/// The extent of the PDO's ability to overload the current for a short period
/// of time.
#[derive(Debug, Clone, Copy, PartialEq, Eq, FromRepr)]
#[repr(u32)]
pub enum PeakCurrent {
    NotSupported = 0b00,
    OverloadLow = 0b01,
    OverloadMedium = 0b10,
    OverloadHigh = 0b11,
}

impl FromStr for PeakCurrent {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        s.parse().ok().and_then(Self::from_repr).ok_or(Error::Parse)
    }
}

/// A number with the unit mV.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Millivolts(pub u32);

impl FromStr for Millivolts {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        parse_unit_suffixed(s, "mV").map(Millivolts)
    }
}

/// A number with the unit mA.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Milliamps(pub u32);

impl FromStr for Milliamps {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        parse_unit_suffixed(s, "mA").map(Milliamps)
    }
}

/// A number with the unit mW.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Milliwatts(pub u32);

impl FromStr for Milliwatts {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        parse_unit_suffixed(s, "mW").map(Milliwatts)
    }
}

#[cfg(test)]
mod tests {
    use googletest::prelude::*;

    use super::*;

    #[test]
    fn test_role_selection_parsing() {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString, Display)]
        #[strum(serialize_all = "snake_case")]
        enum Abc {
            A,
            B,
            C,
        }

        assert_that!(
            RoleSelection::<Abc>::from_str("[a]"),
            ok(eq(RoleSelection {
                role: Abc::A,
                supports_dual: false,
            }))
        );

        assert_that!(
            RoleSelection::<Abc>::from_str("[a] b"),
            ok(eq(RoleSelection {
                role: Abc::A,
                supports_dual: true,
            }))
        );

        assert_that!(
            RoleSelection::<Abc>::from_str("a [b]"),
            ok(eq(RoleSelection {
                role: Abc::B,
                supports_dual: true,
            }))
        );

        assert_that!(
            RoleSelection::<Abc>::from_str("a b [c]"),
            ok(eq(RoleSelection {
                role: Abc::C,
                supports_dual: true,
            }))
        );
    }
}
