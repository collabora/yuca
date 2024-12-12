use googletest::prelude::*;
use rstest::*;
use std::{
    io::ErrorKind as IoErrorKind,
    ops::Deref,
    sync::{LazyLock, Mutex, MutexGuard},
};
use umockdev::prelude::*;

use yuca::{sysfs::*, types::*, Error};

const UMOCKDEV_DATA_COMMON: &str = include_str!("../umockdev/common.umockdev");

macro_rules! property_sysfs {
    (&$($ty:ident)::+$(.$method:tt())+, $($target:tt)*) => {
        result_of!(|x: &$($ty)::+| x$(.$method())+.get(), $($target)*)
    };
}

// Running >1 umockdev test in parallel results in races, so use a lock to make
// sure that doesn't happen.
static TESTBED_LOCK: LazyLock<Mutex<()>> = LazyLock::new(|| Mutex::new(()));

struct LockingTestbed {
    testbed: umockdev::Testbed,
    _guard: MutexGuard<'static, ()>,
}

impl Deref for LockingTestbed {
    type Target = umockdev::Testbed;

    fn deref(&self) -> &Self::Target {
        &self.testbed
    }
}

#[fixture]
fn testbed() -> LockingTestbed {
    let _guard = TESTBED_LOCK.lock().unwrap();

    let testbed = umockdev::Testbed::new();
    // umockdev creates typec devices under /sys/bus, so symlink it to the
    // expected path.
    std::os::unix::fs::symlink(
        format!("{}/bus/typec/devices", testbed.sys_dir()),
        format!("{}/class/typec", testbed.sys_dir()),
    )
    .unwrap();
    return LockingTestbed { testbed, _guard };
}

#[rstest]
fn test_port(testbed: LockingTestbed) {
    testbed.add_from_string(UMOCKDEV_DATA_COMMON).unwrap();

    let mut ports = Port::collection().unwrap().list_opened().unwrap();
    ports.sort_by_key(|p| p.path().port);

    assert_that!(
        ports,
        elements_are![
            all![
                property!(&Port.path(), eq(&PortPath { port: 0 })),
                property_sysfs!(
                    &Port.data_role(),
                    ok(eq(RoleSelection {
                        role: DataRole::Device,
                        supports_dual: true
                    }))
                ),
                property_sysfs!(
                    &Port.power_role(),
                    ok(eq(RoleSelection {
                        role: PowerRole::Sink,
                        supports_dual: true
                    }))
                ),
                property_sysfs!(&Port.preferred_role(), ok(some(eq(PowerRole::Source)))),
                property_sysfs!(
                    &Port.power_operation_mode(),
                    ok(eq(PowerOperationMode::PowerDelivery))
                ),
                property_sysfs!(
                    &Port.usb_power_delivery_revision(),
                    ok(eq(Revision { major: 0, minor: 0 }))
                ),
                property_sysfs!(
                    &Port.usb_typec_revision(),
                    ok(eq(Revision { major: 0, minor: 0 }))
                ),
            ],
            all![
                property!(&Port.path(), eq(&PortPath { port: 1 })),
                property_sysfs!(
                    &Port.data_role(),
                    ok(eq(RoleSelection {
                        role: DataRole::Host,
                        supports_dual: true
                    }))
                ),
                property_sysfs!(
                    &Port.power_role(),
                    ok(eq(RoleSelection {
                        role: PowerRole::Source,
                        supports_dual: true
                    }))
                ),
                property_sysfs!(&Port.preferred_role(), ok(some(eq(PowerRole::Source)))),
                property_sysfs!(
                    &Port.power_operation_mode(),
                    ok(eq(PowerOperationMode::PowerDelivery))
                ),
                property_sysfs!(
                    &Port.usb_power_delivery_revision(),
                    ok(eq(Revision { major: 0, minor: 0 }))
                ),
                property_sysfs!(
                    &Port.usb_typec_revision(),
                    ok(eq(Revision { major: 0, minor: 0 }))
                ),
            ],
        ]
    );
}

#[rstest]
fn test_port_alt_mode(testbed: LockingTestbed) {
    type AltModePort = AltMode<PortPath>;
    type AltModePathPort = AltModePath<PortPath>;

    testbed.add_from_string(UMOCKDEV_DATA_COMMON).unwrap();

    let collection = Port::collection().unwrap();

    let mut alt_modes = collection
        .get(0)
        .unwrap()
        .alt_modes()
        .list_opened()
        .unwrap();
    alt_modes.sort_by_key(|m| m.path().index);

    assert_that!(
        alt_modes,
        elements_are![
            all![
                property!(
                    &AltModePort.path(),
                    eq(&AltModePathPort {
                        parent: PortPath { port: 0 },
                        index: 0
                    })
                ),
                property_sysfs!(&AltModePort.active(), ok(is_true())),
                property_sysfs!(&AltModePort.mode(), ok(eq(1))),
                property_sysfs!(&AltModePort.svid(), ok(eq(0xff01))),
                property_sysfs!(&AltModePort.vdo(), ok(eq(0xc42))),
            ],
            all![
                property!(
                    &AltModePort.path(),
                    eq(&AltModePathPort {
                        parent: PortPath { port: 0 },
                        index: 1
                    })
                ),
                property_sysfs!(&AltModePort.active(), ok(is_true())),
                property_sysfs!(&AltModePort.mode(), ok(eq(7))),
                property_sysfs!(&AltModePort.svid(), ok(eq(0x8087))),
                property_sysfs!(&AltModePort.vdo(), ok(eq(0x0))),
            ],
        ],
    );

    let mut alt_modes = collection
        .get(1)
        .unwrap()
        .alt_modes()
        .list_opened()
        .unwrap();
    alt_modes.sort_by_key(|m| m.path().index);

    assert_that!(
        alt_modes,
        elements_are![
            all![
                property!(
                    &AltModePort.path(),
                    eq(&AltModePathPort {
                        parent: PortPath { port: 1 },
                        index: 0
                    })
                ),
                property_sysfs!(&AltModePort.active(), ok(is_true())),
                property_sysfs!(&AltModePort.mode(), ok(eq(1))),
                property_sysfs!(&AltModePort.svid(), ok(eq(0xff01))),
                property_sysfs!(&AltModePort.vdo(), ok(eq(0xc42))),
            ],
            all![
                property!(
                    &AltModePort.path(),
                    eq(&AltModePathPort {
                        parent: PortPath { port: 1 },
                        index: 1
                    })
                ),
                property_sysfs!(&AltModePort.active(), ok(is_true())),
                property_sysfs!(&AltModePort.mode(), ok(eq(7))),
                property_sysfs!(&AltModePort.svid(), ok(eq(0x8087))),
                property_sysfs!(&AltModePort.vdo(), ok(eq(0x0))),
            ],
        ],
    );
}

#[rstest]
fn test_port_cable(testbed: LockingTestbed) {
    testbed.add_from_string(UMOCKDEV_DATA_COMMON).unwrap();

    let collection = Port::collection().unwrap();

    assert_that!(
        collection.get(0).unwrap().cable().open().unwrap(),
        all![
            property!(&Cable.path(), eq(&CablePath { port: 0 })),
            property_sysfs!(&Cable.cable_type(), ok(eq(CableType::Passive))),
            property_sysfs!(&Cable.plug_type(), ok(eq(PlugType::TypeC))),
            property_sysfs!(
                &Cable.usb_power_delivery_revision(),
                ok(eq(Revision { major: 3, minor: 0 }))
            ),
            property_sysfs!(
                &Cable.identity().id_header(),
                ok(all![
                    field!(
                        VdoIdHeaderCable.0,
                        all![
                            property!(VdoIdHeader.usb_host(), is_false()),
                            property!(VdoIdHeader.usb_device(), is_false()),
                            property!(VdoIdHeader.supports_alt_modes(), is_false()),
                            property!(VdoIdHeader.connector_type(), eq(ConnectorType::Unknown(0))),
                            property!(VdoIdHeader.vendor_id(), eq(0)),
                        ]
                    ),
                    property!(
                        VdoIdHeaderCable.product_type(),
                        eq(ProductTypeCable::Passive)
                    ),
                ])
            ),
            property_sysfs!(
                &Cable.identity().cert_stat(),
                err(eq(Error::IdentityUnavailable))
            ),
            property_sysfs!(
                &Cable.identity().product(),
                err(eq(Error::IdentityUnavailable))
            ),
            property_sysfs!(&Cable.identity().product_type_vdo1(), ok(eq(0x00084840))),
            property_sysfs!(&Cable.identity().product_type_vdo2(), ok(eq(0x0))),
            property_sysfs!(&Cable.identity().product_type_vdo3(), ok(eq(0x0))),
        ],
    );

    assert_that!(
        collection.get(1).unwrap().cable().open().unwrap(),
        all![
            property!(&Cable.path(), eq(&CablePath { port: 1 })),
            property_sysfs!(&Cable.cable_type(), ok(eq(CableType::Passive))),
            property_sysfs!(&Cable.plug_type(), ok(eq(PlugType::TypeC))),
            property_sysfs!(
                &Cable.usb_power_delivery_revision(),
                ok(eq(Revision { major: 3, minor: 0 }))
            ),
            property_sysfs!(
                &Cable.identity().id_header(),
                ok(all![
                    field!(
                        VdoIdHeaderCable.0,
                        all![
                            property!(VdoIdHeader.usb_host(), is_false()),
                            property!(VdoIdHeader.usb_device(), is_false()),
                            property!(VdoIdHeader.supports_alt_modes(), is_false()),
                            property!(VdoIdHeader.connector_type(), eq(ConnectorType::Unknown(0))),
                            property!(VdoIdHeader.vendor_id(), eq(0)),
                        ]
                    ),
                    property!(
                        VdoIdHeaderCable.product_type(),
                        eq(ProductTypeCable::Passive)
                    ),
                ])
            ),
            property_sysfs!(
                &Cable.identity().cert_stat(),
                err(eq(Error::IdentityUnavailable))
            ),
            property_sysfs!(
                &Cable.identity().product(),
                err(eq(Error::IdentityUnavailable))
            ),
            property_sysfs!(&Cable.identity().product_type_vdo1(), ok(eq(0x00082840))),
            property_sysfs!(&Cable.identity().product_type_vdo2(), ok(eq(0x0))),
            property_sysfs!(&Cable.identity().product_type_vdo3(), ok(eq(0x0))),
        ],
    );
}

#[rstest]
fn test_port_cable_plug(testbed: LockingTestbed) {
    testbed.add_from_string(UMOCKDEV_DATA_COMMON).unwrap();

    let collection = Port::collection().unwrap();

    let mut plugs = collection
        .get(0)
        .unwrap()
        .cable()
        .open()
        .unwrap()
        .plugs()
        .list_opened()
        .unwrap();
    plugs.sort_by_key(|m| m.path().plug);

    assert_that!(
        plugs,
        elements_are![all![
            property!(&Plug.path(), eq(&PlugPath { port: 0, plug: 0 })),
            // Nothing else to check for now.
        ]],
    );

    let mut plugs = collection
        .get(1)
        .unwrap()
        .cable()
        .open()
        .unwrap()
        .plugs()
        .list_opened()
        .unwrap();
    plugs.sort_by_key(|m| m.path().plug);

    assert_that!(
        plugs,
        elements_are![all![
            property!(&Plug.path(), eq(&PlugPath { port: 1, plug: 0 })),
            // Nothing else to check for now.
        ]],
    );
}

#[rstest]
fn test_port_cable_plug_alt_mode(testbed: LockingTestbed) {
    testbed.add_from_string(UMOCKDEV_DATA_COMMON).unwrap();

    let collection = Port::collection().unwrap();

    let alt_modes = collection
        .get(0)
        .unwrap()
        .cable()
        .open()
        .unwrap()
        .plugs()
        .get(0)
        .unwrap()
        .alt_modes()
        .list_opened()
        .unwrap();
    assert_that!(alt_modes, empty());

    let alt_modes = collection
        .get(1)
        .unwrap()
        .cable()
        .open()
        .unwrap()
        .plugs()
        .get(0)
        .unwrap()
        .alt_modes()
        .list_opened()
        .unwrap();
    assert_that!(alt_modes, empty());
}

#[rstest]
fn test_port_partner(testbed: LockingTestbed) {
    testbed.add_from_string(UMOCKDEV_DATA_COMMON).unwrap();

    let collection = Port::collection().unwrap();

    assert_that!(
        collection.get(0).unwrap().partner().open().unwrap(),
        all![
            property!(&Partner.path(), eq(&PartnerPath { port: 0 })),
            property_sysfs!(
                &Partner.usb_power_delivery_revision(),
                ok(eq(Revision { major: 3, minor: 0 }))
            ),
            property_sysfs!(
                &Partner.identity().id_header(),
                ok(all![
                    field!(
                        VdoIdHeaderPartner.0,
                        all![
                            property!(VdoIdHeader.usb_host(), is_true()),
                            property!(VdoIdHeader.usb_device(), is_true()),
                            property!(VdoIdHeader.supports_alt_modes(), is_true()),
                            property!(VdoIdHeader.connector_type(), eq(ConnectorType::Unknown(0))),
                            property!(VdoIdHeader.vendor_id(), eq(0x05ac)),
                        ]
                    ),
                    property!(
                        VdoIdHeaderPartner.product_type_ufp(),
                        eq(ProductTypePartnerUfp::PdUsbPeripheral)
                    ),
                    property!(
                        VdoIdHeaderPartner.product_type_dfp(),
                        eq(ProductTypePartnerDfp::PdUsbHost)
                    ),
                ])
            ),
            property_sysfs!(
                &Partner.identity().cert_stat(),
                err(eq(Error::IdentityUnavailable))
            ),
            property_sysfs!(
                &Partner.identity().product(),
                ok(eq(VdoProduct {
                    product_id: 0x7309,
                    bcd_device: 0x2170,
                }))
            ),
            property_sysfs!(&Partner.identity().product_type_vdo1(), ok(eq(0xd00003b))),
            property_sysfs!(&Partner.identity().product_type_vdo2(), ok(eq(0x0))),
            property_sysfs!(&Partner.identity().product_type_vdo3(), ok(eq(0x7000000))),
        ],
    );

    assert_that!(
        collection.get(1).unwrap().partner().open().unwrap(),
        all![
            property!(&Partner.path(), eq(&PartnerPath { port: 1 })),
            property_sysfs!(
                &Partner.usb_power_delivery_revision(),
                ok(eq(Revision { major: 3, minor: 0 }))
            ),
            property_sysfs!(
                &Partner.identity().id_header(),
                ok(all![
                    field!(
                        VdoIdHeaderPartner.0,
                        all![
                            property!(VdoIdHeader.usb_host(), is_true()),
                            property!(VdoIdHeader.usb_device(), is_true()),
                            property!(VdoIdHeader.supports_alt_modes(), is_false()),
                            property!(VdoIdHeader.connector_type(), eq(ConnectorType::Receptacle)),
                            property!(VdoIdHeader.vendor_id(), eq(0x18d1)),
                        ]
                    ),
                    property!(
                        VdoIdHeaderPartner.product_type_ufp(),
                        eq(ProductTypePartnerUfp::PdUsbPeripheral)
                    ),
                    property!(
                        VdoIdHeaderPartner.product_type_dfp(),
                        eq(ProductTypePartnerDfp::PdUsbHost)
                    ),
                ])
            ),
            property_sysfs!(
                &Partner.identity().cert_stat(),
                err(eq(Error::IdentityUnavailable))
            ),
            property_sysfs!(
                &Partner.identity().product(),
                ok(eq(VdoProduct {
                    product_id: 0x4ee1,
                    bcd_device: 0x0
                }))
            ),
            property_sysfs!(&Partner.identity().product_type_vdo1(), ok(eq(0x45800001))),
            property_sysfs!(&Partner.identity().product_type_vdo2(), ok(eq(0x0))),
            property_sysfs!(&Partner.identity().product_type_vdo3(), ok(eq(0x23800000))),
        ],
    );
}

#[rstest]
fn test_port_partner_alt_mode(testbed: LockingTestbed) {
    type AltModePartner = AltMode<PartnerPath>;
    type AltModePathPartner = AltModePath<PartnerPath>;

    testbed.add_from_string(UMOCKDEV_DATA_COMMON).unwrap();

    let collection = Port::collection().unwrap();

    let mut alt_modes = collection
        .get(0)
        .unwrap()
        .partner()
        .open()
        .unwrap()
        .alt_modes()
        .list_opened()
        .unwrap();
    alt_modes.sort_by_key(|m| m.path().index);

    assert_that!(
        alt_modes,
        elements_are![
            all![
                property!(
                    &AltModePartner.path(),
                    eq(&AltModePathPartner {
                        parent: PartnerPath { port: 0 },
                        index: 0
                    })
                ),
                property_sysfs!(&AltModePartner.active(), ok(is_false())),
                property_sysfs!(&AltModePartner.mode(), ok(eq(1))),
                property_sysfs!(&AltModePartner.svid(), ok(eq(0x8087))),
                property_sysfs!(&AltModePartner.vdo(), ok(eq(0x1))),
            ],
            all![
                property!(
                    &AltModePartner.path(),
                    eq(&AltModePathPartner {
                        parent: PartnerPath { port: 0 },
                        index: 1
                    })
                ),
                property_sysfs!(&AltModePartner.active(), ok(is_false())),
                property_sysfs!(&AltModePartner.mode(), ok(eq(1))),
                property_sysfs!(&AltModePartner.svid(), ok(eq(0xff01))),
                property_sysfs!(&AltModePartner.vdo(), ok(eq(0x1c46))),
            ],
            all![
                property!(
                    &AltModePartner.path(),
                    eq(&AltModePathPartner {
                        parent: PartnerPath { port: 0 },
                        index: 2
                    })
                ),
                property_sysfs!(&AltModePartner.active(), ok(is_false())),
                property_sysfs!(&AltModePartner.mode(), ok(eq(1))),
                property_sysfs!(&AltModePartner.svid(), ok(eq(0x0451))),
                property_sysfs!(&AltModePartner.vdo(), ok(eq(0x3))),
            ],
            all![
                property!(
                    &AltModePartner.path(),
                    eq(&AltModePathPartner {
                        parent: PartnerPath { port: 0 },
                        index: 3
                    })
                ),
                property_sysfs!(&AltModePartner.active(), ok(is_false())),
                property_sysfs!(&AltModePartner.mode(), ok(eq(1))),
                property_sysfs!(&AltModePartner.svid(), ok(eq(0x05ac))),
                property_sysfs!(&AltModePartner.vdo(), ok(eq(0x1))),
            ],
            all![
                property!(
                    &AltModePartner.path(),
                    eq(&AltModePathPartner {
                        parent: PartnerPath { port: 0 },
                        index: 4
                    })
                ),
                property_sysfs!(&AltModePartner.active(), ok(is_false())),
                property_sysfs!(&AltModePartner.mode(), ok(eq(2))),
                property_sysfs!(&AltModePartner.svid(), ok(eq(0x05ac))),
                property_sysfs!(&AltModePartner.vdo(), ok(eq(0x2))),
            ],
        ],
    );

    let alt_modes = collection
        .get(1)
        .unwrap()
        .partner()
        .open()
        .unwrap()
        .alt_modes()
        .list_opened()
        .unwrap();
    assert_that!(alt_modes, empty());
}

#[rstest]
fn test_port_partner_pd(testbed: LockingTestbed) {
    testbed.add_from_string(UMOCKDEV_DATA_COMMON).unwrap();

    let collection = Port::collection().unwrap();

    let mut pds = collection
        .get(0)
        .unwrap()
        .partner()
        .open()
        .unwrap()
        .pds()
        .list_opened()
        .unwrap();
    pds.sort_by_key(|m| m.path().pd);

    assert_that!(
        pds,
        elements_are![all![
            property!(
                &PowerDelivery.path(),
                eq(&PowerDeliveryPath { port: 0, pd: 0 })
            ),
            // Nothing else to check for now.
        ]],
    );

    let mut pds = collection
        .get(1)
        .unwrap()
        .partner()
        .open()
        .unwrap()
        .pds()
        .list_opened()
        .unwrap();
    pds.sort_by_key(|m| m.path().pd);

    assert_that!(
        pds,
        elements_are![all![
            property!(
                &PowerDelivery.path(),
                eq(&PowerDeliveryPath { port: 1, pd: 1 })
            ),
            // Nothing else to check for now.
        ]],
    );
}

#[rstest]
fn test_port_partner_source_pdo(testbed: LockingTestbed) {
    testbed.add_from_string(UMOCKDEV_DATA_COMMON).unwrap();

    let collection = Port::collection().unwrap();

    let mut pdos = collection
        .get(0)
        .unwrap()
        .partner()
        .open()
        .unwrap()
        .pds()
        .get(0)
        .unwrap()
        .source_capabilities()
        .open()
        .unwrap()
        .pdos()
        .list_opened()
        .unwrap();
    pdos.sort_by_key(|m| m.path().index);

    assert_that!(
        pdos,
        elements_are![all![
            property!(
                &SourcePdo.path(),
                eq(&PdoPath {
                    port: 0,
                    pd: 0,
                    index: 1,
                    role: PowerRole::Source,
                    supply: SupplyKind::FixedSupply
                })
            ),
            matches_pattern!(SourcePdo::FixedSupply(all![
                property_sysfs!(&SourcePdoFixedSupply.dual_role_power(), ok(is_true())),
                property_sysfs!(
                    &SourcePdoFixedSupply.usb_suspend_supported(),
                    ok(is_false())
                ),
                property_sysfs!(&SourcePdoFixedSupply.unconstrained_power(), ok(is_true())),
                property_sysfs!(
                    &SourcePdoFixedSupply.usb_communication_capable(),
                    ok(is_true())
                ),
                property_sysfs!(&SourcePdoFixedSupply.dual_role_data(), ok(is_true())),
                property_sysfs!(
                    &SourcePdoFixedSupply.unchunked_extended_messages_supported(),
                    ok(is_true())
                ),
                property_sysfs!(&SourcePdoFixedSupply.voltage(), ok(eq(Millivolts(5000)))),
                property_sysfs!(
                    &SourcePdoFixedSupply.peak_current(),
                    err(eq(Error::Io(IoErrorKind::NotFound)))
                ),
                property_sysfs!(
                    &SourcePdoFixedSupply.maximum_current(),
                    ok(eq(Milliamps(3000)))
                ),
            ])),
        ]],
    );

    let mut pdos = collection
        .get(1)
        .unwrap()
        .partner()
        .open()
        .unwrap()
        .pds()
        .get(1)
        .unwrap()
        .source_capabilities()
        .open()
        .unwrap()
        .pdos()
        .list_opened()
        .unwrap();
    pdos.sort_by_key(|m| m.path().index);

    assert_that!(
        pdos,
        elements_are![all![
            property!(
                &SourcePdo.path(),
                eq(&PdoPath {
                    port: 1,
                    pd: 1,
                    index: 1,
                    role: PowerRole::Source,
                    supply: SupplyKind::FixedSupply
                })
            ),
            matches_pattern!(SourcePdo::FixedSupply(all![
                property_sysfs!(&SourcePdoFixedSupply.dual_role_power(), ok(is_true())),
                property_sysfs!(&SourcePdoFixedSupply.usb_suspend_supported(), ok(is_true())),
                property_sysfs!(&SourcePdoFixedSupply.unconstrained_power(), ok(is_false())),
                property_sysfs!(
                    &SourcePdoFixedSupply.usb_communication_capable(),
                    ok(is_true())
                ),
                property_sysfs!(&SourcePdoFixedSupply.dual_role_data(), ok(is_true())),
                property_sysfs!(
                    &SourcePdoFixedSupply.unchunked_extended_messages_supported(),
                    ok(is_false())
                ),
                property_sysfs!(&SourcePdoFixedSupply.voltage(), ok(eq(Millivolts(5000)))),
                property_sysfs!(
                    &SourcePdoFixedSupply.peak_current(),
                    err(eq(Error::Io(IoErrorKind::NotFound)))
                ),
                property_sysfs!(
                    &SourcePdoFixedSupply.maximum_current(),
                    ok(eq(Milliamps(900)))
                ),
            ])),
        ]],
    );
}

#[rstest]
fn test_port_partner_sink_pdo(testbed: LockingTestbed) {
    testbed.add_from_string(UMOCKDEV_DATA_COMMON).unwrap();

    let collection = Port::collection().unwrap();

    let mut pdos = collection
        .get(0)
        .unwrap()
        .partner()
        .open()
        .unwrap()
        .pds()
        .get(0)
        .unwrap()
        .sink_capabilities()
        .open()
        .unwrap()
        .pdos()
        .list_opened()
        .unwrap();
    pdos.sort_by_key(|m| m.path().index);

    assert_that!(
        pdos,
        elements_are![
            all![
                property!(
                    &SinkPdo.path(),
                    eq(&PdoPath {
                        port: 0,
                        pd: 0,
                        index: 1,
                        role: PowerRole::Sink,
                        supply: SupplyKind::FixedSupply
                    })
                ),
                matches_pattern!(SinkPdo::FixedSupply(all![
                    property_sysfs!(&SinkPdoFixedSupply.dual_role_power(), ok(is_true())),
                    property_sysfs!(&SinkPdoFixedSupply.higher_capability(), ok(is_false())),
                    property_sysfs!(&SinkPdoFixedSupply.unconstrained_power(), ok(is_true())),
                    property_sysfs!(
                        &SinkPdoFixedSupply.usb_communication_capable(),
                        ok(is_true())
                    ),
                    property_sysfs!(
                        &SinkPdoFixedSupply.unchunked_extended_messages_supported(),
                        ok(is_false())
                    ),
                    property_sysfs!(&SinkPdoFixedSupply.fast_role_swap_current(), ok(eq(0))),
                    property_sysfs!(&SinkPdoFixedSupply.voltage(), ok(eq(Millivolts(5000)))),
                    property_sysfs!(
                        &SinkPdoFixedSupply.operational_current(),
                        ok(eq(Milliamps(3000)))
                    ),
                ])),
            ],
            all![
                property!(
                    &SinkPdo.path(),
                    eq(&PdoPath {
                        port: 0,
                        pd: 0,
                        index: 2,
                        role: PowerRole::Sink,
                        supply: SupplyKind::VariableSupply
                    })
                ),
                matches_pattern!(SinkPdo::VariableSupply(all![
                    property_sysfs!(
                        &SinkPdoVariableSupply.maximum_voltage(),
                        ok(eq(Millivolts(21000)))
                    ),
                    property_sysfs!(
                        &SinkPdoVariableSupply.minimum_voltage(),
                        ok(eq(Millivolts(4750)))
                    ),
                    property_sysfs!(
                        &SinkPdoVariableSupply.operational_current(),
                        ok(eq(Milliamps(4700)))
                    ),
                ])),
            ],
        ],
    );

    let mut pdos = collection
        .get(1)
        .unwrap()
        .partner()
        .open()
        .unwrap()
        .pds()
        .get(1)
        .unwrap()
        .sink_capabilities()
        .open()
        .unwrap()
        .pdos()
        .list_opened()
        .unwrap();
    pdos.sort_by_key(|m| m.path().index);

    assert_that!(
        pdos,
        elements_are![
            all![
                property!(
                    &SinkPdo.path(),
                    eq(&PdoPath {
                        port: 1,
                        pd: 1,
                        index: 1,
                        role: PowerRole::Sink,
                        supply: SupplyKind::FixedSupply
                    })
                ),
                matches_pattern!(SinkPdo::FixedSupply(all![
                    property_sysfs!(&SinkPdoFixedSupply.dual_role_power(), ok(is_true())),
                    property_sysfs!(&SinkPdoFixedSupply.higher_capability(), ok(is_true())),
                    property_sysfs!(&SinkPdoFixedSupply.unconstrained_power(), ok(is_false())),
                    property_sysfs!(
                        &SinkPdoFixedSupply.usb_communication_capable(),
                        ok(is_true())
                    ),
                    property_sysfs!(
                        &SinkPdoFixedSupply.unchunked_extended_messages_supported(),
                        ok(is_false())
                    ),
                    property_sysfs!(&SinkPdoFixedSupply.fast_role_swap_current(), ok(eq(0))),
                    property_sysfs!(&SinkPdoFixedSupply.voltage(), ok(eq(Millivolts(5000)))),
                    property_sysfs!(
                        &SinkPdoFixedSupply.operational_current(),
                        ok(eq(Milliamps(3000)))
                    ),
                ])),
            ],
            all![
                property!(
                    &SinkPdo.path(),
                    eq(&PdoPath {
                        port: 1,
                        pd: 1,
                        index: 2,
                        role: PowerRole::Sink,
                        supply: SupplyKind::FixedSupply
                    })
                ),
                matches_pattern!(SinkPdo::FixedSupply(all![
                    property_sysfs!(
                        &SinkPdoFixedSupply.dual_role_power(),
                        err(eq(Error::Io(IoErrorKind::NotFound)))
                    ),
                    property_sysfs!(
                        &SinkPdoFixedSupply.higher_capability(),
                        err(eq(Error::Io(IoErrorKind::NotFound)))
                    ),
                    property_sysfs!(
                        &SinkPdoFixedSupply.unconstrained_power(),
                        err(eq(Error::Io(IoErrorKind::NotFound)))
                    ),
                    property_sysfs!(
                        &SinkPdoFixedSupply.usb_communication_capable(),
                        err(eq(Error::Io(IoErrorKind::NotFound)))
                    ),
                    property_sysfs!(
                        &SinkPdoFixedSupply.unchunked_extended_messages_supported(),
                        err(eq(Error::Io(IoErrorKind::NotFound)))
                    ),
                    property_sysfs!(
                        &SinkPdoFixedSupply.fast_role_swap_current(),
                        err(eq(Error::Io(IoErrorKind::NotFound)))
                    ),
                    property_sysfs!(&SinkPdoFixedSupply.voltage(), ok(eq(Millivolts(9000)))),
                    property_sysfs!(
                        &SinkPdoFixedSupply.operational_current(),
                        ok(eq(Milliamps(2200)))
                    ),
                ])),
            ],
            all![
                property!(
                    &SinkPdo.path(),
                    eq(&PdoPath {
                        port: 1,
                        pd: 1,
                        index: 3,
                        role: PowerRole::Sink,
                        supply: SupplyKind::ProgrammableSupply
                    })
                ),
                matches_pattern!(SinkPdo::ProgrammableSupply(all![
                    property_sysfs!(
                        &SinkPdoProgrammableSupply.maximum_voltage(),
                        ok(eq(Millivolts(11000)))
                    ),
                    property_sysfs!(
                        &SinkPdoProgrammableSupply.minimum_voltage(),
                        ok(eq(Millivolts(5000)))
                    ),
                    property_sysfs!(
                        &SinkPdoProgrammableSupply.maximum_current(),
                        ok(eq(Milliamps(3000)))
                    ),
                ])),
            ],
        ],
    );
}

#[cfg(feature = "tokio")]
mod feat_tokio {
    use camino::Utf8PathBuf;
    use futures_lite::StreamExt;
    use std::time::Duration;
    use tokio::{task::JoinHandle, time::timeout};
    use yuca::watcher::*;

    use super::*;

    const TIMEOUT: Duration = Duration::from_millis(25);

    async fn consume_watcher(w: Watcher, jh: JoinHandle<yuca::Result<()>>) {
        std::mem::drop(w);
        timeout(TIMEOUT, jh).await.unwrap().unwrap().unwrap();
    }

    async fn do_test_watch<P: DevicePathWatchable>(testbed: LockingTestbed, target: P, other: P) {
        testbed.add_from_string(UMOCKDEV_DATA_COMMON).unwrap();

        let (w, jh) = Watcher::spawn_tokio(EventSource::Netlink).unwrap();
        w.enable_umockdev_events_for_testing().unwrap();

        let mut any_added = P::any_added(&w).unwrap();
        let mut any_changed = P::any_changed(&w).unwrap();
        let mut any_removed = P::any_removed(&w).unwrap();

        let mut target_added = target.added(&w).unwrap();
        let mut target_changed = target.changed(&w).unwrap();
        let mut target_removed = target.removed(&w).unwrap();

        let mut other_added = other.added(&w).unwrap();
        let mut other_changed = other.changed(&w).unwrap();
        let mut other_removed = other.removed(&w).unwrap();

        let mut target_sys = Utf8PathBuf::from("/sys/bus/typec/devices");
        target.build_syspath(&mut target_sys);

        testbed.uevent(target_sys.as_str(), "add");
        assert_that!(
            timeout(TIMEOUT, any_added.try_next()).await,
            err(anything()),
        );
        assert_that!(
            timeout(TIMEOUT, target_added.try_next()).await,
            err(anything()),
        );
        assert_that!(
            timeout(TIMEOUT, other_added.try_next()).await,
            err(anything()),
        );

        testbed.uevent(target_sys.as_str(), "bind");
        assert_that!(
            timeout(TIMEOUT, any_added.try_next()).await,
            ok(ok(some(eq(&target))))
        );
        assert_that!(
            timeout(TIMEOUT, target_added.try_next()).await,
            ok(ok(some(eq(&target))))
        );
        assert_that!(
            timeout(TIMEOUT, target_removed.try_next()).await,
            err(anything()),
        );
        assert_that!(
            timeout(TIMEOUT, other_added.try_next()).await,
            err(anything()),
        );

        testbed.uevent(target_sys.as_str(), "change");
        assert_that!(
            timeout(TIMEOUT, any_changed.try_next()).await,
            ok(ok(some(eq(&target))))
        );
        assert_that!(
            timeout(TIMEOUT, target_changed.try_next()).await,
            ok(ok(some(eq(&target))))
        );
        assert_that!(
            timeout(TIMEOUT, other_changed.try_next()).await,
            err(anything()),
        );

        testbed.uevent(target_sys.as_str(), "unbind");
        assert_that!(
            timeout(TIMEOUT, any_removed.try_next()).await,
            ok(ok(some(eq(&target))))
        );
        assert_that!(
            timeout(TIMEOUT, target_removed.try_next()).await,
            ok(ok(some(eq(&target))))
        );
        assert_that!(
            timeout(TIMEOUT, other_removed.try_next()).await,
            err(anything()),
        );

        testbed.uevent(target_sys.as_str(), "remove");
        assert_that!(
            timeout(TIMEOUT, any_removed.try_next()).await,
            err(anything()),
        );
        assert_that!(
            timeout(TIMEOUT, target_removed.try_next()).await,
            err(anything()),
        );
        assert_that!(
            timeout(TIMEOUT, other_removed.try_next()).await,
            err(anything()),
        );

        consume_watcher(w, jh).await;
    }

    async fn do_test_watch_from_parent<Child: DevicePathWatchableFromParent>(
        testbed: LockingTestbed,
        child: Child,
        collection_getter: impl FnOnce(&Child::Parent) -> DevicePathCollection<Child>,
    ) {
        testbed.add_from_string(UMOCKDEV_DATA_COMMON).unwrap();

        let (w, jh) = Watcher::spawn_tokio(EventSource::Netlink).unwrap();
        w.enable_umockdev_events_for_testing().unwrap();

        let collection = collection_getter(&child.parent());
        let mut added = collection.added(&w).unwrap();
        let mut changed = collection.changed(&w).unwrap();
        let mut removed = collection.removed(&w).unwrap();

        let mut child_sys = Utf8PathBuf::from("/sys/bus/typec/devices");
        child.build_syspath(&mut child_sys);

        testbed.uevent(child_sys.as_str(), "add");
        assert_that!(timeout(TIMEOUT, added.try_next()).await, err(anything()),);

        testbed.uevent(child_sys.as_str(), "bind");
        assert_that!(
            timeout(TIMEOUT, added.try_next()).await,
            ok(ok(some(eq(&child))))
        );

        testbed.uevent(child_sys.as_str(), "change");
        assert_that!(
            timeout(TIMEOUT, changed.try_next()).await,
            ok(ok(some(eq(&child))))
        );

        testbed.uevent(child_sys.as_str(), "unbind");
        assert_that!(
            timeout(TIMEOUT, removed.try_next()).await,
            ok(ok(some(eq(&child))))
        );

        testbed.uevent(child_sys.as_str(), "remove");
        assert_that!(timeout(TIMEOUT, removed.try_next()).await, err(anything()));

        consume_watcher(w, jh).await;
    }

    #[rstest]
    #[tokio::test]
    async fn test_watch_port(testbed: LockingTestbed) {
        do_test_watch(testbed, PortPath { port: 0 }, PortPath { port: 1 }).await;
    }

    #[rstest]
    #[tokio::test]
    async fn test_watch_port_from_parent(testbed: LockingTestbed) {
        do_test_watch_from_parent(testbed, PortPath { port: 0 }, |_| PortPath::collection()).await;
    }

    #[rstest]
    #[tokio::test]
    async fn test_watch_port_cable(testbed: LockingTestbed) {
        do_test_watch(testbed, CablePath { port: 0 }, CablePath { port: 1 }).await;
    }

    #[rstest]
    #[tokio::test]
    async fn test_watch_port_cable_plug(testbed: LockingTestbed) {
        do_test_watch(
            testbed,
            PlugPath { port: 0, plug: 0 },
            PlugPath { port: 1, plug: 0 },
        )
        .await;
    }

    #[rstest]
    #[tokio::test]
    async fn test_watch_port_cable_plug_from_parent(testbed: LockingTestbed) {
        do_test_watch_from_parent(testbed, PlugPath { port: 0, plug: 0 }, CablePath::plugs).await;
    }

    // TODO: we have no test data for these!
    // #[rstest]
    // #[tokio::test]
    // async fn test_watch_port_cable_plug_alt_mode(testbed: LockingTestbed) {}

    // #[rstest]
    // #[tokio::test]
    // async fn test_watch_port_cable_plug_alt_mode_from_parent(testbed: LockingTestbed) {}

    #[rstest]
    #[tokio::test]
    async fn test_watch_port_partner(testbed: LockingTestbed) {
        do_test_watch(testbed, PartnerPath { port: 0 }, PartnerPath { port: 1 }).await;
    }

    #[rstest]
    #[tokio::test]
    async fn test_watch_port_partner_alt_mode(testbed: LockingTestbed) {
        do_test_watch(
            testbed,
            AltModePath {
                parent: PartnerPath { port: 0 },
                index: 0,
            },
            AltModePath {
                parent: PartnerPath { port: 0 },
                index: 1,
            },
        )
        .await;
    }

    #[rstest]
    #[tokio::test]
    async fn test_watch_port_partner_alt_mode_from_parent(testbed: LockingTestbed) {
        do_test_watch_from_parent(
            testbed,
            AltModePath {
                parent: PartnerPath { port: 0 },
                index: 0,
            },
            PartnerPath::alt_modes,
        )
        .await;
    }

    #[rstest]
    #[tokio::test]
    async fn test_watch_port_partner_pd(testbed: LockingTestbed) {
        do_test_watch(
            testbed,
            PowerDeliveryPath { port: 0, pd: 0 },
            PowerDeliveryPath { port: 1, pd: 1 },
        )
        .await;
    }

    #[rstest]
    #[tokio::test]
    async fn test_watch_port_partner_pd_from_parent(testbed: LockingTestbed) {
        do_test_watch_from_parent(
            testbed,
            PowerDeliveryPath { port: 0, pd: 0 },
            PartnerPath::pds,
        )
        .await;
    }

    #[rstest]
    #[tokio::test]
    async fn test_watch_overflow(testbed: LockingTestbed) {
        const TOTAL: usize = 256;

        testbed.add_from_string(UMOCKDEV_DATA_COMMON).unwrap();

        let (w, jh) = Watcher::spawn_tokio(EventSource::Netlink).unwrap();
        w.enable_umockdev_events_for_testing().unwrap();

        let p = PortPath { port: 0 };
        let mut sys = Utf8PathBuf::from("/sys/bus/typec/devices");
        p.build_syspath(&mut sys);

        let mut added = p.added(&w).unwrap();

        for _ in 0..TOTAL {
            testbed.uevent(sys.as_str(), "bind");
        }

        let mut received = 0;
        let mut missed = 0;
        while let Ok(event) = timeout(TIMEOUT, added.try_next()).await {
            match event {
                Ok(_) => received += 1,
                Err(Overflowed { missed_events }) => missed += missed_events as usize,
            }
        }

        consume_watcher(w, jh).await;

        assert_that!(received, gt(1));
        assert_that!(missed, gt(1));
        assert_that!((received, missed), result_of!(|(a, b)| a + b, eq(TOTAL)));
    }
}
