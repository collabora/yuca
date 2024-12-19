# yuca

[![crates.io](https://img.shields.io/crates/v/yuca?style=for-the-badge)](https://crates.io/crates/yuca)
[![docs.rs](https://img.shields.io/docsrs/yuca?style=for-the-badge)](https://docs.rs/yuca)
[![build status](https://img.shields.io/github/actions/workflow/status/collabora/yuca/ci.yml?style=for-the-badge)](https://github.com/collabora/yuca/actions/workflows/ci.yml)

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
