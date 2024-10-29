use yuca::sysfs::*;

#[derive(Copy, Clone)]
struct Dumper {
    indent_level: usize,
}

impl Dumper {
    fn indented(self, block: impl FnOnce(Dumper)) {
        block(Dumper {
            indent_level: self.indent_level + 1,
        });
    }
}

macro_rules! dump {
    ($dumper: expr, $fmt: expr $(, $($args: expr),*)? $(,)?) => {
        println!(concat!("{:_indent$}", $fmt), "" $(, $($args),*)?, _indent = $dumper.indent_level * 2)
    }
}

macro_rules! dump_prop {
    ($dumper: expr, $target: expr, $prop: ident $(,)?) => {
        dump!(
            $dumper,
            "{}: {:?}",
            stringify!($prop),
            $target.$prop().get()
        );
    };
}

macro_rules! dump_getter_error {
    ($dumper: expr, $target: expr, $getter: ident $(,)?) => {
        match $target.$getter() {
            Ok(item) => Some(item),
            Err(err) => {
                dump!($dumper, "{}: {:?}", stringify!($getter), err);
                None
            }
        }
    };
}

macro_rules! dump_iter_errors {
    ($dumper: expr, $target: expr, $getter: ident $(,)?) => {
        $target
            .$getter()
            .map_err(move |err| dump!($dumper, "{}: {:?}", stringify!($getter), err))
            .into_iter()
            .flatten()
            .enumerate()
            .filter_map(move |(i, item)| match item {
                Ok(item) => Some(item),
                Err(err) => {
                    dump!($dumper, "{}: #{}: {:?}", stringify!($getter), i, err);
                    None
                }
            })
    };
}

fn main() {
    let dumper = Dumper { indent_level: 0 };

    for port in Port::list().unwrap() {
        let port = port.unwrap();
        dump!(dumper, "Port: {}", port.port());
        dumper.indented(|dumper| {
            dump_prop!(dumper, port, port_type);
            dump_prop!(dumper, port, data_role);
            dump_prop!(dumper, port, power_role);
            dump_prop!(dumper, port, preferred_role);
            dump_prop!(dumper, port, power_operation_mode);
            dump_prop!(dumper, port, usb_power_delivery_revision);
            dump_prop!(dumper, port, usb_typec_revision);

            for alt_mode in dump_iter_errors!(dumper, port, alt_modes) {
                dump!(dumper, "Alt mode: {}", alt_mode.index());
                dumper.indented(|dumper| {
                    dump_prop!(dumper, alt_mode, active);
                    dump_prop!(dumper, alt_mode, supported_roles);
                    dump_prop!(dumper, alt_mode, svid);
                    dump_prop!(dumper, alt_mode, vdo);
                });
            }

            if let Some(cable) = dump_getter_error!(dumper, port, cable) {
                dump!(dumper, "Cable:");
                dumper.indented(|dumper| {
                    dump_prop!(dumper, cable, usb_power_delivery_revision);

                    dump!(dumper, "Identity:");
                    dumper.indented(|dumper| {
                        let identity = cable.identity();
                        dump_prop!(dumper, identity, id_header);
                        dump_prop!(dumper, identity, cert_stat);
                        dump_prop!(dumper, identity, product);
                        dump_prop!(dumper, identity, product_type_vdo1);
                        dump_prop!(dumper, identity, product_type_vdo2);
                        dump_prop!(dumper, identity, product_type_vdo3);
                    });
                });
            }

            if let Some(partner) = dump_getter_error!(dumper, port, partner) {
                dump!(dumper, "Partner:");
                dumper.indented(|dumper| {
                    dump_prop!(dumper, partner, usb_power_delivery_revision);

                    dump!(dumper, "Identity:");
                    dumper.indented(|dumper| {
                        let identity = partner.identity();
                        dump_prop!(dumper, identity, id_header);
                        dump_prop!(dumper, identity, cert_stat);
                        dump_prop!(dumper, identity, product);
                        dump_prop!(dumper, identity, product_type_vdo1);
                        dump_prop!(dumper, identity, product_type_vdo2);
                        dump_prop!(dumper, identity, product_type_vdo3);
                    });

                    dump_prop!(dumper, partner, usb_power_delivery_revision);
                    for alt_mode in dump_iter_errors!(dumper, partner, alt_modes) {
                        dump!(dumper, "Alt mode: {}", alt_mode.index());
                        dumper.indented(|dumper| {
                            dump_prop!(dumper, alt_mode, active);
                            dump_prop!(dumper, alt_mode, supported_roles);
                            dump_prop!(dumper, alt_mode, svid);
                            dump_prop!(dumper, alt_mode, vdo);
                        });
                    }
                });
            }

            if let Some(pd) = dump_getter_error!(dumper, port, usb_power_delivery) {
                dump!(dumper, "USB PD:");
                dumper.indented(|dumper| {
                    dump!(dumper, "Source capabilities:");
                    dumper.indented(|dumper| {
                        for caps in dump_iter_errors!(dumper, pd, source_capabilities) {
                            match caps {
                                SourceCapabilities::FixedSupply(caps) => {
                                    dump!(dumper, "- Fixed supply:");
                                    dumper.indented(|dumper| {
                                        dump_prop!(dumper, caps, dual_role_power);
                                        dump_prop!(dumper, caps, usb_suspend_supported);
                                        dump_prop!(dumper, caps, unconstrained_power);
                                        dump_prop!(dumper, caps, usb_communication_capable);
                                        dump_prop!(dumper, caps, dual_role_data);
                                        dump_prop!(
                                            dumper,
                                            caps,
                                            unchuncked_extended_messages_supported
                                        );
                                        dump_prop!(dumper, caps, peak_current);
                                        dump_prop!(dumper, caps, voltage);
                                        dump_prop!(dumper, caps, maximum_current);
                                    });
                                }
                                SourceCapabilities::Battery(caps) => {
                                    dump!(dumper, "- Battery:");
                                    dumper.indented(|dumper| {
                                        dump_prop!(dumper, caps, maximum_voltage);
                                        dump_prop!(dumper, caps, minimum_voltage);
                                        dump_prop!(dumper, caps, maximum_power);
                                    });
                                }
                                SourceCapabilities::VariableSupply(caps) => {
                                    dump!(dumper, "- Variable supply:");
                                    dumper.indented(|dumper| {
                                        dump_prop!(dumper, caps, maximum_voltage);
                                        dump_prop!(dumper, caps, minimum_voltage);
                                        dump_prop!(dumper, caps, maximum_current);
                                    });
                                }
                                SourceCapabilities::ProgrammableSupply(caps) => {
                                    dump!(dumper, "- Programmable supply:");
                                    dumper.indented(|dumper| {
                                        dump_prop!(dumper, caps, power_limited);
                                        dump_prop!(dumper, caps, maximum_voltage);
                                        dump_prop!(dumper, caps, minimum_voltage);
                                        dump_prop!(dumper, caps, maximum_current);
                                    });
                                }
                            }
                        }
                    });
                });

                dumper.indented(|dumper| {
                    dump!(dumper, "Sink capabilities:");
                    dumper.indented(|dumper| {
                        for caps in dump_iter_errors!(dumper, pd, sink_capabilities) {
                            match caps {
                                SinkCapabilities::FixedSupply(caps) => {
                                    dump!(dumper, "- Fixed supply:");
                                    dumper.indented(|dumper| {
                                        dump_prop!(dumper, caps, dual_role_power);
                                        dump_prop!(dumper, caps, higher_capability);
                                        dump_prop!(dumper, caps, unconstrained_power);
                                        dump_prop!(dumper, caps, usb_communication_capable);
                                        dump_prop!(dumper, caps, dual_role_data);
                                        dump_prop!(dumper, caps, unchuncked_external_messages);
                                        dump_prop!(dumper, caps, fast_role_swap_current);
                                        dump_prop!(dumper, caps, voltage);
                                        dump_prop!(dumper, caps, operational_current);
                                    });
                                }
                                SinkCapabilities::Battery(caps) => {
                                    dump!(dumper, "- Battery:");
                                    dumper.indented(|dumper| {
                                        dump_prop!(dumper, caps, maximum_voltage);
                                        dump_prop!(dumper, caps, minimum_voltage);
                                        dump_prop!(dumper, caps, operational_power);
                                    });
                                }
                                SinkCapabilities::VariableSupply(caps) => {
                                    dump!(dumper, "- Variable supply:");
                                    dumper.indented(|dumper| {
                                        dump_prop!(dumper, caps, maximum_voltage);
                                        dump_prop!(dumper, caps, minimum_voltage);
                                        dump_prop!(dumper, caps, operational_current);
                                    });
                                }
                                SinkCapabilities::ProgrammableSupply(caps) => {
                                    dump!(dumper, "- Programmable supply:");
                                    dumper.indented(|dumper| {
                                        dump_prop!(dumper, caps, maximum_voltage);
                                        dump_prop!(dumper, caps, minimum_voltage);
                                        dump_prop!(dumper, caps, maximum_current);
                                    });
                                }
                            }
                        }
                    });
                });
            }
        });
    }
}
