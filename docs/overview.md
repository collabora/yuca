# Overview

```text
                           +----------------------+
                           |         Port         |
                           |       PortPath       |
                           +----------------------+
                                      |
            +------------------------+ +--------------------+
            |                         |                     |
            v                         v                     v
  +-------------------------+    +-------------+    +---------------+
  |    AltMode<PortPath>    |    |    Cable    |    |    Partner    |
  |  AltModePath<PortPath>  |    |  CablePath  |    |  PartnerPath  |
  +-------------------------+    +-------------+    +---------------+
                                        |                   |
                +-----------------------+       +----------+ +--------------+
                |                               |                           |
                v                               v                           v
         +--------------+            +---------------------+  +----------------------------+
         |     Plug     |            |    PowerDelivery    |  |    AltMode<PartnerPath>    |
         |   PlugPath   |            |  PowerDeliveryPath  |  |  AltModePath<PartnerPath>  |
         +--------------+            +---------------------+  +----------------------------+
                |                               |
                v                          +---+ +------------------+
   +-------------------------+             |                        |
   |    AltMode<PlugPath>    |             v                        v
   |  AltModePath<PlugPath>  |  +----------------------+  +--------------------+
   +-------------------------+  |  SourceCapabilities  |  |  SinkCapabilities  |
                                |   CapabilitiesPath   |  |  CapabilitiesPath  |
                                +----------------------+  +--------------------+
                                           |                        |
                                           v                        v
                                 +-------------------+    +-------------------+
                                 |     SourcePdo     |    |      SinkPdo      |
                                 |      PdoPath      |    |      PdoPath      |
                                 |                   |    |                   |
                                 |  -FixedSupply     |    |  -FixedSupply     |
                                 |  -Battery         |    |  -Battery         |
                                 |  -VariableSupply  |    |  -VariableSupply  |
                                 |  -ProgramSupply   |    |  -ProgramSupply   |
                                 +-------------------+    +-------------------+



```

## [`Device`]s and [`DevicePath`]s

The above diagram has two lines for each node: a [`Device`] and its
[`DevicePath`], respectively.

[`DevicePath`]s are structured types that can be converted on-demand to and
from subdirectories of `/sys/class/typec`. For instance,
`/sys/class/typec/port0/port0-partner` is a `PortPath { port: 0 }`.

[`Device`]s, on the other hand, are what you get when you actually open a
[`DevicePath`] and grants access to its properties. If the device becomes
unavailable at any point, then accessing the properties will start returning
errors.

**NOTE:** [`Device`]s and [`DevicePath`]s **do not necessarily correspond to
physical devices**. Some of them, such as [`Port`]s, do correspond to a
physical counterpart, but others like [`PowerDelivery`] map to USB-C concepts
instead.

### [`DeviceEntry`]

A [`DeviceEntry`] is simply an unopened child of another device:

```rust
# use yuca::{*, sysfs::*};
# fn get_a_port_from_somewhere() -> Port { todo!() }
# fn test() -> Result<()> {
let port: Port = get_a_port_from_somewhere();

let partner_entry: DeviceEntry<'_, Partner> = port.partner();
let partner_path: &PartnerPath = partner_entry.path();
let partner: Partner = partner_entry.open()?;
# Ok(())
# }
```

### [`DeviceCollection`]

A [`DeviceCollection`] is a handle that lets you iterate over multiple child
[`DeviceEntry`]s:

```rust
# use yuca::{*, sysfs::*};
# fn test() -> Result<()> {
let ports: DeviceCollection<'_, Port> = Port::collection()?;

let port0: Port = ports.get(0)?;

for entry in ports.iter()? {
    let entry: DeviceEntry<'_, Port> = entry?;
    let path: &PortPath = entry.path();
    let port: Port = entry.open()?;

    let alt_modes: DeviceCollection<'_, AltMode<PortPath>> = port.alt_modes();
}

let ports_vec: Vec<DeviceEntry<'_, Port>> = ports.list()?;
# Ok(())
# }
```

If you know you're going to immediately call [`DeviceEntry::open`], then you
can just use [`DeviceCollection::iter_opened`] or
[`DeviceCollection::list_opened`]:

```rust
# use yuca::{*, sysfs::*};
# fn test() -> Result<()> {
let ports: DeviceCollection<'_, Port> = Port::collection()?;

for port in ports.iter_opened()? {
    let port: Port = port?;
}

let ports_vec: Vec<Port> = ports.list_opened()?;
# Ok(())
# }
```

### [`DevicePathCollection`]

A [`DevicePathCollection`] is akin to a [`DeviceCollection`] but instead creates
child paths relative to a parent:

```rust
# use yuca::{*, sysfs::*};
# fn test() -> Result<()> {
let ports: DevicePathCollection<PortPath> = PortPath::collection();

let port0: PortPath = ports.get(0);
let port01 = port0.alt_modes().get(1);
# Ok(())
# }
```

(Note that these paths, by nature of being paths, do not necessarily correspond
to a device that exists on disk.)

### Opening paths directly

If you already have a [`DevicePath`] that you'd like to immediately open, you
can use [`Device::open`]:

```rust
# use yuca::{*, sysfs::*};
# fn test() -> Result<()> {
let port: Port = Port::open(PortPath { port: 0 })?;
# Ok(())
# }
```

**NOTE:** You may wonder what the purpose of the entire [`DeviceCollection`]
and [`DeviceEntry`] API for getting single devices if you could just construct
a [`DevicePath`] and open it individually. The answer is that, in most cases,
you don't actually know a single device you're interested in ahead of time, and
[`DeviceCollection`]s provide a race-free way to slowly make your way down the
tree and observe interesting devices.

## Properties

[`Device`]s all have a variety of *properties* on them, which are
implementations of either [`PropertyReadable`] or [`PropertyWritable`] returned
by getter methods. This is how you can access various information about the
devices:

```rust
# use yuca::{*, types::*, sysfs::*};
# fn test() -> Result<()> {
let port: Port = Port::collection()?.get(0)?;
let data_role: RoleSelection<DataRole> = port.data_role().get()?;
let power_role: RoleSelection<PowerRole> = port.power_role().get()?;
println!("Port 0's data role is: {data_role:?}");
println!("Port 0's power role is: {power_role:?}");
# Ok(())
# }
```

[`Device`]: trait@sysfs::Device
[`Device::open`]: fn@sysfs::Device::open
[`DeviceEntry`]: struct@sysfs::DeviceEntry
[`DeviceEntry::open`]: fn@sysfs::DeviceEntry::open
[`DevicePath`]: trait@sysfs::DevicePath
[`DeviceCollection`]: struct@sysfs::DeviceCollection
[`DeviceCollection::iter`]: fn@sysfs::DeviceCollection::iter
[`DeviceCollection::iter_opened`]: fn@sysfs::DeviceCollection::iter_opened
[`DeviceCollection::list`]: fn@sysfs::DeviceCollection::list
[`DeviceCollection::list_opened`]: fn@sysfs::DeviceCollection::list_opened
[`DevicePathCollection`]: struct@sysfs::DeviceCollection
[`Port`]: struct@sysfs::Port
[`PowerDelivery`]: struct@sysfs::PowerDelivery
[`PropertyReadable`]: trait@sysfs::PropertyReadable
[`PropertyWritable`]: trait@sysfs::PropertyWritable
