# yuca

Yuca is a Rust crate to access USB Type-C device information on Linux.

```rust,no_run
use std::error::Error;
use futures_lite::StreamExt;
use yuca::{sysfs::*, watcher::*};

fn show() -> Result<(), Box<dyn Error>> {
    for port in Port::collection()?.list()? {
        let port = port.open()?;

        println!("Port: {}", port.path().port);

        let Ok(partner) = port.partner().open() else { continue; };
        println!("  Partner: {}:{}",
            partner.identity().id_header().get()?.0.vendor_id(),
            partner.identity().product().get()?.product_id);
    }

    Ok(())
}

async fn watch() -> Result<(), Box<dyn Error>> {
    let (w, _) = Watcher::spawn_tokio(EventSource::Netlink)?;
    let mut stream = PartnerPath::any_added(&w)?;
    while let Some(path) = stream.next().await {
        let Ok(path) = path else { continue; };
        println!("Partner added to port: {}", path.port);
    }
    Ok(())
}
```

## Terminology

This crate uses a variety of terms from the USB Power Delivery specification,
which can be obtained from [here](https://www.usb.org/document-library/usb-power-delivery).
Consult the spec's "Terms and Abbreviations" for complete definitions.
