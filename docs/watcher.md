# Watching for devices.

Use [`Watcher`], obtained via [`Watcher::spawn_tokio`] for Tokio users, to watch
events for added, changed, and removed devices (currently this crate does not
support `udev`, only raw netlink) that implement [`DevicePathWatchable`]:

```rust
# use futures_lite::StreamExt;
# use yuca::{*, sysfs::*, watcher::*};
# async fn test() -> Result<()> {
let (w, _) = Watcher::spawn_tokio(EventSource::Netlink)?;

let mut stream = PartnerPath::any_added(&w).unwrap();
while let Some(path) = stream.next().await {
    let Ok(path) = path else {
        eprintln!("Stream overflowed! We were too slow.");
        continue;
    };
    assert_eq!(path, PartnerPath { port: 0 });
    println!("Partner to port {} was added!", path.port);
}
# Ok(())
# }
```

or watch for specific paths:

```rust
# use futures_lite::StreamExt;
# use yuca::{*, sysfs::*, watcher::*};
# async fn test() -> Result<()> {
let (w, _) = Watcher::spawn_tokio(EventSource::Netlink)?;

let mut stream = PartnerPath {port: 0}.removed(&w).unwrap();
while let Some(path) = stream.next().await {
    let Ok(path) = path else {
        eprintln!("Stream overflowed! We were too slow.");
        continue;
    };
    let path: PartnerPath = path;
    println!("port0-partner was removed!");
}
# Ok(())
# }
```

You can also get events for only the relevant children of a given path:

```rust
# use futures_lite::StreamExt;
# use yuca::{*, sysfs::*, watcher::*};
# async fn test() -> Result<()> {
let (w, _) = Watcher::spawn_tokio(EventSource::Netlink)?;

let parent = CablePath { port: 0 };
let mut stream = parent.plugs().changed(&w).unwrap();
while let Some(path) = stream.next().await {
    let Ok(path) = path else {
        eprintln!("Stream overflowed! We were too slow.");
        continue;
    };
    assert_eq!(path.port, parent.port);
    println!("Plug {} on port {} was changed!", path.plug, path.port);
}
# Ok(())
# }
```

## Custom event loops

In order to watch devices without Tokio, you need to use
[`Watcher::new_with_manual_dispatcher`] and then poll the returned
[`EventDispatcher`] in the background. Consult the implementation of
[`Watcher::spawn_tokio`] for an example.

[`DevicePathWatchable`]: trait@crate::sysfs::DevicePathWatchable
[`EventDispatcher`]: struct@EventDispatcher
[`Watcher`]: struct@Watcher
[`Watcher::new_with_manual_dispatcher`]: fn@Watcher::new_with_manual_dispatcher
[`Watcher::spawn_tokio`]: fn@Watcher::new_with_manual_dispatcher
