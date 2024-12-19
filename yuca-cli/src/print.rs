use std::fmt;

use owo_colors::Stream::Stdout;
use usb_ids::FromId;
use yuca::{sysfs::*, types::*};

use crate::styles::{self, StyleApply};

pub enum DeviceOpener<'a, T: Device> {
    Entry(DeviceEntry<'a, T>),
    Path(T::Path),
}

impl<T: Device> DeviceOpener<'_, T> {
    fn path(&self) -> &T::Path {
        match self {
            DeviceOpener::Entry(entry) => entry.path(),
            DeviceOpener::Path(path) => path,
        }
    }

    pub fn open(&self) -> yuca::Result<T> {
        match self {
            DeviceOpener::Entry(entry) => entry.open(),
            DeviceOpener::Path(path) => T::open(*path),
        }
    }
}

#[derive(Clone, Copy)]
enum TreeLevel {
    Property(&'static str),
    Child,
}

#[derive(Clone, Copy)]
enum TreePos {
    Root,
    Child {
        parent_children: usize,
        parent_props: usize,
        level: TreeLevel,
    },
}

macro_rules! styled_fmt {
    ($style:ident, $($args:tt)*) => {
        format_args!($($args)*).style_if_supported(Stdout, styles::$style())
    }
}

macro_rules! print_line {
    ($printer:expr, $($args:tt)*) => {
        $printer.line(format_args!($($args)*))
    }
}

macro_rules! print_line_styled {
    ($printer:expr, $style:ident, $($args:tt)*) => {
        print_line!($printer, "{}", styled_fmt!($style, $($args)*))
    }
}

#[derive(Clone, Copy)]
pub struct TreePrinter {
    pos: TreePos,
    recursive: bool,
    show_errors: bool,
}

impl TreePrinter {
    pub fn shallow_root() -> Self {
        Self {
            pos: TreePos::Root,
            recursive: false,
            show_errors: false,
        }
    }

    pub fn recursive(&self) -> Self {
        Self {
            recursive: true,
            ..*self
        }
    }

    pub fn show_errors(&self) -> Self {
        Self {
            show_errors: true,
            ..*self
        }
    }

    fn level(&self, level: TreeLevel) -> Self {
        TreePrinter {
            pos: match self.pos {
                TreePos::Child {
                    parent_children,
                    parent_props,
                    level: TreeLevel::Child,
                } => TreePos::Child {
                    parent_children: parent_children + 1,
                    parent_props,
                    level,
                },
                TreePos::Child {
                    parent_children,
                    parent_props,
                    level: TreeLevel::Property(_),
                } => TreePos::Child {
                    parent_children,
                    parent_props: parent_props + 1,
                    level,
                },
                TreePos::Root => TreePos::Child {
                    parent_children: 0,
                    parent_props: 0,
                    level,
                },
            },
            ..*self
        }
    }

    pub fn line(&self, args: fmt::Arguments<'_>) {
        if let TreePos::Child {
            parent_children,
            parent_props,
            level,
        } = self.pos
        {
            for _ in 0..parent_children {
                print!("│   ");
            }
            for _ in 0..parent_props {
                print!("    ");
            }

            match level {
                TreeLevel::Property(name) => print!(
                    "{}   {} ",
                    styled_fmt!(property_star, "✱"),
                    styled_fmt!(label, "{name}:")
                ),
                TreeLevel::Child => print!("│── "),
            }
        }

        println!("{}", args);
    }

    pub fn property(&self, name: &'static str) -> Self {
        self.level(TreeLevel::Property(name))
    }

    pub fn child(&self) -> Self {
        self.level(TreeLevel::Child)
    }

    pub fn maybe_show_error<T>(&self, result: yuca::Result<T>) -> yuca::Result<T> {
        if let Err(err) = &result {
            if self.show_errors {
                print_line!(
                    self,
                    "{} {}",
                    styled_fmt!(error_label, "error:"),
                    styled_fmt!(error_body, "{}", err)
                );
            }
        }

        result
    }

    pub fn try_open<T>(
        &self,
        prefix: impl fmt::Display,
        result: yuca::Result<T>,
    ) -> yuca::Result<T> {
        if let Err(err) = &result {
            if self.show_errors {
                print_line!(
                    self,
                    "{}{} {} {}",
                    styled_fmt!(tree_opening, "{prefix}"),
                    // Style this separately, in case 'prefix' changed the
                    // active styles.
                    styled_fmt!(tree_opening, ":"),
                    styled_fmt!(error_label, "error:"),
                    styled_fmt!(error_body, "{err}")
                );
            }
        } else {
            print_line!(
                self,
                "{}{}",
                styled_fmt!(tree_opening, "{prefix}"),
                // Style this separately, in case 'prefix' changed the active
                // styles.
                styled_fmt!(tree_opening, ":")
            );
        }

        result
    }

    pub fn try_apply<T, R>(&self, result: yuca::Result<T>, cb: impl FnOnce(Self, T) -> R) {
        let _ = self.maybe_show_error(result).map(|ok| cb(*self, ok));
    }

    pub fn collection<Child: Device>(
        &self,
        prefix: impl fmt::Display,
        collection: DeviceCollection<'_, Child>,
        cb: impl Fn(Self, DeviceOpener<'_, Child>),
    ) {
        assert!(self.recursive);

        if let Ok(iter) = self.try_open(prefix, collection.iter()) {
            let mut none = true;
            let p = self.child();
            for child in iter {
                let Ok(child) = p.maybe_show_error(child) else {
                    continue;
                };

                cb(p, DeviceOpener::Entry(child));
                none = false;
            }

            if none {
                print_line_styled!(p, dim, "(none)");
            }
        }
    }
}

pub fn print_port(p: TreePrinter, port: DeviceOpener<'_, Port>) {
    let Ok(port) = p.show_errors().try_open(
        format_args!(
            "Port {}",
            styled_fmt!(device_index, "#{}", port.path().port)
        ),
        port.open(),
    ) else {
        return;
    };

    p.property("Data role")
        .try_apply(port.data_role().get(), |p, value| {
            print_line!(
                p,
                "{}{}",
                styled_fmt!(literal, "{}", value.role),
                styled_fmt!(
                    special,
                    "{}",
                    if value.supports_dual { " (dual)" } else { "" }
                )
            )
        });

    p.property("Port type")
        .try_apply(port.port_type().get(), |p, value| {
            print_line!(
                p,
                "{}{}",
                styled_fmt!(literal, "{}", value.role),
                styled_fmt!(
                    special,
                    "{}",
                    if value.supports_dual && value.role != PortType::Dual {
                        " (dual)"
                    } else {
                        ""
                    }
                )
            )
        });

    p.property("Power role")
        .try_apply(port.power_role().get(), |p, value| {
            print_line!(
                p,
                "{}{}",
                styled_fmt!(literal, "{}", value.role),
                styled_fmt!(
                    special,
                    "{}",
                    if value.supports_dual { " (dual)" } else { "" }
                )
            )
        });

    p.property("Preferred role")
        .try_apply(port.preferred_role().get(), |p, value| {
            if let Some(role) = value {
                print_line_styled!(p, literal, "{role}");
            } else {
                print_line_styled!(p, special, "none");
            }
        });

    p.property("Power operation mode")
        .try_apply(port.power_operation_mode().get(), |p, value| match value {
            PowerOperationMode::Default => print_line_styled!(p, number, "default"),
            PowerOperationMode::TypeC1_5A => print_line!(
                p,
                "{} {}",
                styled_fmt!(literal, "Type-C"),
                styled_fmt!(number, "1.5A @ 5V")
            ),
            PowerOperationMode::TypeC3_0A => print_line!(
                p,
                "{} {}",
                styled_fmt!(literal, "Type-C"),
                styled_fmt!(number, "3.0A @ 5V")
            ),
            PowerOperationMode::PowerDelivery => print_line_styled!(p, literal, "USB PD"),
        });

    p.property("USB Type-C revision")
        .try_apply(port.usb_typec_revision().get(), |p, value| {
            print_line_styled!(p, number, "{}.{}", value.major, value.minor)
        });

    p.property("USB PD revision")
        .try_apply(port.usb_power_delivery_revision().get(), |p, value| {
            print_line_styled!(p, number, "{}.{}", value.major, value.minor)
        });

    if p.recursive {
        p.child()
            .collection("Alternate modes", port.alt_modes(), print_alt_mode_port);
        print_cable(p.child(), DeviceOpener::Entry(port.cable()));
        print_partner(p.child(), DeviceOpener::Entry(port.partner()));
    }
}

fn print_alt_mode_common(p: TreePrinter, alt_mode: &AltMode<impl DevicePath>) {
    p.property("Active")
        .try_apply(alt_mode.active().get(), |p, value| {
            print_line_styled!(p, special, "{value}")
        });

    p.property("Mode")
        .try_apply(alt_mode.mode().get(), |p, value| {
            print_line_styled!(p, number, "{value}")
        });

    p.property("SVID")
        .try_apply(alt_mode.svid().get(), |p, value| {
            print_vendor(p, value);
        });

    p.property("VDO")
        .try_apply(alt_mode.vdo().get(), |p, value| {
            print_line_styled!(p, number, "0x{:x}", value)
        });
}

pub fn print_alt_mode_generic(
    p: TreePrinter,
    alt_mode: DeviceOpener<'_, AltMode<impl DevicePath>>,
) {
    let Ok(alt_mode) = p.show_errors().try_open(
        format_args!(
            "Alternate mode {}",
            styled_fmt!(device_index, "#{}", alt_mode.path().index)
        ),
        alt_mode.open(),
    ) else {
        return;
    };

    print_alt_mode_common(p, &alt_mode);
}

pub fn print_alt_mode_port(p: TreePrinter, alt_mode: DeviceOpener<'_, AltMode<PortPath>>) {
    let Ok(alt_mode) = p.show_errors().try_open(
        format_args!(
            "Alternate mode {}",
            styled_fmt!(device_index, "#{}", alt_mode.path().index)
        ),
        alt_mode.open(),
    ) else {
        return;
    };

    print_alt_mode_common(p, &alt_mode);

    p.property("Supported roles")
        .try_apply(alt_mode.supported_roles().get(), |p, value| {
            match (value.source, value.sink) {
                (true, true) => print_line_styled!(p, literal, "source, sink"),
                (true, false) => print_line_styled!(p, literal, "source"),
                (false, true) => print_line_styled!(p, literal, "sink"),
                // XXX: is this possible?
                (false, false) => print_line_styled!(p, literal, "none"),
            }
        });
}

pub fn print_cable(p: TreePrinter, cable: DeviceOpener<'_, Cable>) {
    let Ok(cable) = p.try_open("Cable", cable.open()) else {
        return;
    };

    p.property("Type")
        .try_apply(cable.cable_type().get(), |p, value| match value {
            CableType::NotCable => (),
            CableType::Passive => print_line_styled!(p, literal, "passive"),
            CableType::Active => print_line_styled!(p, literal, "active"),
            CableType::Vpd => print_line_styled!(p, literal, "VCONN-powered device"),
        });

    p.property("Plug type")
        .try_apply(cable.plug_type().get(), |p, value| match value {
            PlugType::Unknown => print_line_styled!(p, literal, "unknown"),
            PlugType::TypeA => print_line_styled!(p, literal, "Type-A"),
            PlugType::TypeB => print_line_styled!(p, literal, "Type-B"),
            PlugType::TypeC => print_line_styled!(p, literal, "Type-C"),
            PlugType::Captive => print_line_styled!(p, literal, "Captive"),
        });

    p.property("USB PD revision")
        .try_apply(cable.usb_power_delivery_revision().get(), |p, value| {
            print_line_styled!(p, number, "{}.{}", value.major, value.minor)
        });

    let header = cable.identity().id_header().get();
    p.property("Identity header")
        .try_apply(header, |p, header| {
            print_line!(p, "");

            print_id_header_common(p, header.0);

            {
                let p = p.property("Product type");
                match header.product_type() {
                    ProductTypeCable::NotCable => (),
                    ProductTypeCable::Passive => print_line_styled!(p, literal, "passive"),
                    ProductTypeCable::Active => print_line_styled!(p, literal, "active"),
                    ProductTypeCable::Vpd => print_line_styled!(p, literal, "VCONN-powered device"),
                    ProductTypeCable::Unknown(other) => print_line!(
                        p,
                        "{} {}",
                        styled_fmt!(literal, "unknown"),
                        styled_fmt!(number, "({other})")
                    ),
                }
            }
        });

    p.property("USB-IF certification XID")
        .try_apply(cable.identity().cert_stat().get(), |p, value| {
            print_line_styled!(p, number, "0x{:x}", value.0)
        });

    p.property("Product")
        .try_apply(cable.identity().product().get(), |p, value| {
            print_product(p, header.ok().map(|header| header.0), value);
            print_line_styled!(p.property("Revision"), number, "0x{:x}", value.bcd_device);
        });

    p.property("VDO 1")
        .try_apply(cable.identity().product_type_vdo1().get(), |p, value| {
            print_line_styled!(p, number, "0x{:x}", value)
        });
    p.property("VDO 2")
        .try_apply(cable.identity().product_type_vdo2().get(), |p, value| {
            print_line_styled!(p, number, "0x{:x}", value)
        });
    p.property("VDO 3")
        .try_apply(cable.identity().product_type_vdo3().get(), |p, value| {
            print_line_styled!(p, number, "0x{:x}", value)
        });

    if p.recursive {
        p.child().collection("Plugs", cable.plugs(), print_plug);
    }
}

pub fn print_plug(p: TreePrinter, plug: DeviceOpener<'_, Plug>) {
    let Ok(plug) = p.show_errors().try_open(
        format_args!(
            "Plug {}",
            styled_fmt!(device_index, "#{}", plug.path().plug)
        ),
        plug.open(),
    ) else {
        return;
    };

    if p.recursive {
        p.child()
            .collection("Alternate modes", plug.alt_modes(), print_alt_mode_generic);
    }
}

pub fn print_partner(p: TreePrinter, partner: DeviceOpener<'_, Partner>) {
    let Ok(partner) = p.try_open("Partner", partner.open()) else {
        return;
    };

    p.property("USB PD revision")
        .try_apply(partner.usb_power_delivery_revision().get(), |p, value| {
            print_line_styled!(p, number, "{}.{}", value.major, value.minor)
        });

    let header = partner.identity().id_header().get();
    p.property("Identity header")
        .try_apply(header, |p, header| {
            print_line!(p, "");

            print_id_header_common(p, header.0);

            {
                let p = p.property("Upstream-facing port");
                match header.product_type_ufp() {
                    ProductTypePartnerUfp::NotUfp => (),
                    ProductTypePartnerUfp::PdUsbHub => print_line_styled!(p, literal, "USB hub"),
                    ProductTypePartnerUfp::PdUsbPeripheral => {
                        print_line_styled!(p, literal, "USB peripheral")
                    }
                    ProductTypePartnerUfp::Psd => print_line_styled!(p, literal, "Power-only sink"),
                    ProductTypePartnerUfp::Unknown(other) => print_line!(
                        p,
                        "{} {}",
                        styled_fmt!(literal, "unknown"),
                        styled_fmt!(number, "({other})")
                    ),
                }
            }

            {
                let p = p.property("Downstream-facing port");
                match header.product_type_dfp() {
                    ProductTypePartnerDfp::NotDfp => (),
                    ProductTypePartnerDfp::PdUsbHub => print_line_styled!(p, literal, "USB hub"),
                    ProductTypePartnerDfp::PdUsbHost => print_line_styled!(p, literal, "USB host"),
                    ProductTypePartnerDfp::PowerBrick => {
                        print_line_styled!(p, literal, "Power brick")
                    }
                    ProductTypePartnerDfp::Unknown(other) => print_line!(
                        p,
                        "{} {}",
                        styled_fmt!(literal, "unknown"),
                        styled_fmt!(number, "({other})")
                    ),
                }
            }
        });

    p.property("USB-IF certification XID")
        .try_apply(partner.identity().cert_stat().get(), |p, value| {
            print_line_styled!(p, number, "0x{:x}", value.0)
        });

    p.property("Product")
        .try_apply(partner.identity().product().get(), |p, value| {
            print_product(p, header.ok().map(|header| header.0), value);
            print_line_styled!(p.property("Revision"), number, "0x{:x}", value.bcd_device);
        });

    p.property("VDO 1")
        .try_apply(partner.identity().product_type_vdo1().get(), |p, value| {
            print_line_styled!(p, number, "0x{:x}", value)
        });
    p.property("VDO 2")
        .try_apply(partner.identity().product_type_vdo2().get(), |p, value| {
            print_line_styled!(p, number, "0x{:x}", value)
        });
    p.property("VDO 3")
        .try_apply(partner.identity().product_type_vdo3().get(), |p, value| {
            print_line_styled!(p, number, "0x{:x}", value)
        });

    if p.recursive {
        p.child().collection(
            "Alternate modes",
            partner.alt_modes(),
            print_alt_mode_generic,
        );
        p.child().collection("PDs", partner.pds(), print_pd);
    }
}

pub fn print_pd(p: TreePrinter, pd: DeviceOpener<'_, PowerDelivery>) {
    let Ok(pd) = p.try_open(
        format_args!("PD {}", styled_fmt!(device_index, "#{}", pd.path().pd)),
        pd.open(),
    ) else {
        return;
    };

    // This has nothing interesting except for the capabilities children.
    assert!(p.recursive);

    {
        let p = p.child();
        if let Ok(caps) = p
            .property("Source capabilities")
            .maybe_show_error(pd.source_capabilities().open())
        {
            p.collection("Source PDOs", caps.pdos(), print_source_pdo);
        }
        if let Ok(caps) = p
            .property("Sink capabilities")
            .maybe_show_error(pd.sink_capabilities().open())
        {
            p.collection("Sink PDOs", caps.pdos(), print_sink_pdo);
        }
    }
}

fn print_source_pdo(p: TreePrinter, pdo: DeviceOpener<'_, SourcePdo>) {
    let Ok(pdo) = p.try_open(
        format_args!(
            "Source PDO {}",
            styled_fmt!(device_index, "#{}", pdo.path().index)
        ),
        pdo.open(),
    ) else {
        return;
    };

    match pdo {
        SourcePdo::FixedSupply(pdo) => {
            let pdo = match pdo.try_into_vsafe5v() {
                Ok(pdo) => {
                    print_line!(
                        p.property("Type"),
                        "{} {}",
                        styled_fmt!(literal, "fixed"),
                        styled_fmt!(special, "(vSafe5V)")
                    );

                    p.property("Dual role power")
                        .try_apply(pdo.dual_role_power().get(), |p, value| {
                            print_line_styled!(p, special, "{value}")
                        });
                    p.property("USB suspend supported")
                        .try_apply(pdo.usb_suspend_supported().get(), |p, value| {
                            print_line_styled!(p, special, "{value}")
                        });
                    p.property("Unconstrained power")
                        .try_apply(pdo.unconstrained_power().get(), |p, value| {
                            print_line_styled!(p, special, "{value}")
                        });
                    p.property("USB communication capable")
                        .try_apply(pdo.usb_communication_capable().get(), |p, value| {
                            print_line_styled!(p, special, "{value}")
                        });
                    p.property("Dual role data")
                        .try_apply(pdo.dual_role_data().get(), |p, value| {
                            print_line_styled!(p, special, "{value}")
                        });
                    p.property("Unchunked extended messages supported")
                        .try_apply(
                            pdo.unchunked_extended_messages_supported().get(),
                            |p, value| print_line_styled!(p, special, "{value}"),
                        );

                    pdo.into_fixed_supply()
                }
                Err(pdo) => {
                    print_line_styled!(p.property("Type"), literal, "fixed");
                    pdo
                }
            };

            p.property("Peak current overload")
                .try_apply(pdo.peak_current().get(), |p, value| match value {
                    PeakCurrent::NotSupported => print_line_styled!(p, literal, "not supported"),
                    PeakCurrent::OverloadLow => print_line_styled!(p, literal, "low"),
                    PeakCurrent::OverloadMedium => print_line_styled!(p, literal, "medium"),
                    PeakCurrent::OverloadHigh => print_line_styled!(p, literal, "medium"),
                });
            p.property("Voltage")
                .try_apply(pdo.voltage().get(), |p, value| {
                    print_line_styled!(p, number, "{} mV", value.0)
                });
            p.property("Maximum current")
                .try_apply(pdo.maximum_current().get(), |p, value| {
                    print_line_styled!(p, number, "{} mA", value.0)
                });
        }
        SourcePdo::Battery(pdo) => {
            print_line_styled!(p.property("Type"), literal, "battery");
            p.property("Maximum voltage")
                .try_apply(pdo.maximum_voltage().get(), |p, value| {
                    print_line_styled!(p, number, "{} mV", value.0)
                });
            p.property("Minimum voltage")
                .try_apply(pdo.minimum_voltage().get(), |p, value| {
                    print_line_styled!(p, number, "{} mV", value.0)
                });
            p.property("Maximum power")
                .try_apply(pdo.maximum_power().get(), |p, value| {
                    print_line_styled!(p, number, "{} mW", value.0)
                });
        }
        SourcePdo::VariableSupply(pdo) => {
            print_line_styled!(p.property("Type"), literal, "variable-voltage supply");
            p.property("Maximum voltage")
                .try_apply(pdo.maximum_voltage().get(), |p, value| {
                    print_line_styled!(p, number, "{} mV", value.0)
                });
            p.property("Minimum voltage")
                .try_apply(pdo.minimum_voltage().get(), |p, value| {
                    print_line_styled!(p, number, "{} mV", value.0)
                });
            p.property("Maximum current")
                .try_apply(pdo.maximum_current().get(), |p, value| {
                    print_line_styled!(p, number, "{} mA", value.0)
                });
        }
        SourcePdo::ProgrammableSupply(pdo) => {
            print_line_styled!(p.property("Type"), literal, "programmable supply");
            p.property("Power limited")
                .try_apply(pdo.power_limited().get(), |p, value| {
                    print_line_styled!(p, special, "{value}")
                });
            p.property("Maximum voltage")
                .try_apply(pdo.maximum_voltage().get(), |p, value| {
                    print_line_styled!(p, number, "{} mV", value.0)
                });
            p.property("Minimum voltage")
                .try_apply(pdo.minimum_voltage().get(), |p, value| {
                    print_line_styled!(p, number, "{} mV", value.0)
                });
            p.property("Maximum current")
                .try_apply(pdo.maximum_current().get(), |p, value| {
                    print_line_styled!(p, number, "{} mA", value.0)
                });
        }
    }
}

fn print_sink_pdo(p: TreePrinter, pdo: DeviceOpener<'_, SinkPdo>) {
    let Ok(pdo) = p.try_open(
        format_args!(
            "Sink PDO {}",
            styled_fmt!(device_index, "#{}", pdo.path().index)
        ),
        pdo.open(),
    ) else {
        return;
    };

    match pdo {
        SinkPdo::FixedSupply(pdo) => {
            let pdo = match pdo.try_into_vsafe5v() {
                Ok(pdo) => {
                    print_line!(
                        p.property("Type"),
                        "{} {}",
                        styled_fmt!(literal, "fixed"),
                        styled_fmt!(special, "(vSafe5V)")
                    );

                    p.property("Dual role power")
                        .try_apply(pdo.dual_role_power().get(), |p, value| {
                            print_line_styled!(p, special, "{value}")
                        });
                    p.property("Higher capability")
                        .try_apply(pdo.higher_capability().get(), |p, value| {
                            print_line_styled!(p, special, "{value}")
                        });
                    p.property("Unconstrained power")
                        .try_apply(pdo.unconstrained_power().get(), |p, value| {
                            print_line_styled!(p, special, "{value}")
                        });
                    p.property("USB communication capable")
                        .try_apply(pdo.usb_communication_capable().get(), |p, value| {
                            print_line_styled!(p, special, "{value}")
                        });
                    p.property("Dual role data")
                        .try_apply(pdo.dual_role_data().get(), |p, value| {
                            print_line_styled!(p, special, "{value}")
                        });
                    p.property("Unchunked extended messages supported")
                        .try_apply(
                            pdo.unchunked_extended_messages_supported().get(),
                            |p, value| print_line_styled!(p, special, "{value}"),
                        );
                    p.property("Fast role swap").try_apply(
                        pdo.fast_role_swap_current().get(),
                        |p, value| match value {
                            FastRoleSwapCurrent::NotSupported => {
                                print_line_styled!(p, literal, "not supported")
                            }
                            FastRoleSwapCurrent::Default => {
                                print_line_styled!(p, literal, "default")
                            }
                            FastRoleSwapCurrent::_1_5A => {
                                print_line_styled!(p, number, "1.5A @ 5V")
                            }
                            FastRoleSwapCurrent::_3_0A => {
                                print_line_styled!(p, number, "3.0A @ 5V")
                            }
                        },
                    );

                    pdo.into_fixed_supply()
                }
                Err(pdo) => {
                    print_line_styled!(p.property("Type"), literal, "fixed");
                    pdo
                }
            };

            p.property("Voltage")
                .try_apply(pdo.voltage().get(), |p, value| {
                    print_line_styled!(p, number, "{} mV", value.0)
                });
            p.property("Operational current")
                .try_apply(pdo.operational_current().get(), |p, value| {
                    print_line_styled!(p, number, "{} mA", value.0)
                });
        }
        SinkPdo::Battery(pdo) => {
            print_line_styled!(p.property("Type"), literal, "battery");
            p.property("Maximum voltage")
                .try_apply(pdo.maximum_voltage().get(), |p, value| {
                    print_line_styled!(p, number, "{} mV", value.0)
                });
            p.property("Minimum voltage")
                .try_apply(pdo.minimum_voltage().get(), |p, value| {
                    print_line_styled!(p, number, "{} mV", value.0)
                });
            p.property("Operational power")
                .try_apply(pdo.operational_power().get(), |p, value| {
                    print_line_styled!(p, number, "{} mW", value.0)
                });
        }
        SinkPdo::VariableSupply(pdo) => {
            print_line_styled!(p.property("Type"), literal, "variable-voltage supply");
            p.property("Maximum voltage")
                .try_apply(pdo.maximum_voltage().get(), |p, value| {
                    print_line_styled!(p, number, "{} mV", value.0)
                });
            p.property("Minimum voltage")
                .try_apply(pdo.minimum_voltage().get(), |p, value| {
                    print_line_styled!(p, number, "{} mV", value.0)
                });
            p.property("Operational current")
                .try_apply(pdo.operational_current().get(), |p, value| {
                    print_line_styled!(p, number, "{} mA", value.0)
                });
        }
        SinkPdo::ProgrammableSupply(pdo) => {
            print_line_styled!(p.property("Type"), literal, "programmable supply");
            p.property("Maximum voltage")
                .try_apply(pdo.maximum_voltage().get(), |p, value| {
                    print_line_styled!(p, number, "{} mV", value.0)
                });
            p.property("Minimum voltage")
                .try_apply(pdo.minimum_voltage().get(), |p, value| {
                    print_line_styled!(p, number, "{} mV", value.0)
                });
            p.property("Maximum current")
                .try_apply(pdo.maximum_current().get(), |p, value| {
                    print_line_styled!(p, number, "{} mA", value.0)
                });
        }
    }
}

fn print_id_header_common(p: TreePrinter, header: VdoIdHeader) {
    print_line_styled!(p.property("USB host"), special, "{}", header.usb_host());
    print_line_styled!(p.property("USB device"), special, "{}", header.usb_device());
    print_line_styled!(
        p.property("Supports alternate modes"),
        special,
        "{}",
        header.supports_alt_modes()
    );

    {
        let p = p.property("Connector type");
        match header.connector_type() {
            ConnectorType::Receptacle => print_line_styled!(p, literal, "receptacle"),
            ConnectorType::Plug => print_line_styled!(p, literal, "plug"),
            ConnectorType::Unknown(other) => print_line!(
                p,
                "{} {}",
                styled_fmt!(literal, "unknown"),
                styled_fmt!(number, "({other})")
            ),
        }
    }

    {
        let p = p.property("Vendor");
        print_vendor(p, header.vendor_id());
    }
}

fn print_vendor(p: TreePrinter, vendor_id: u16) {
    if let Some(vendor) = usb_ids::Vendor::from_id(vendor_id) {
        print_line!(
            p,
            "{} {}",
            styled_fmt!(literal, "{}", vendor.name()),
            styled_fmt!(number, "(0x{:04x})", vendor_id)
        );
    } else {
        print_line_styled!(p, number, "0x{:04x}", vendor_id)
    }
}

fn print_product(p: TreePrinter, header: Option<VdoIdHeader>, value: VdoProduct) {
    if let Some(dev) = header
        .and_then(|header| usb_ids::Device::from_vid_pid(header.vendor_id(), value.product_id))
    {
        print_line!(
            p,
            "{} {}",
            styled_fmt!(literal, "{}", dev.name()),
            styled_fmt!(number, "(0x{:04x})", value.product_id)
        );
    } else {
        print_line_styled!(p, number, "0x{:04x}", value.product_id);
    }
}
