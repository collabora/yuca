mod print;
mod styles;

use std::future::Future;

use bpaf::{Bpaf, Parser};
use camino::Utf8PathBuf;
use futures_lite::StreamExt;
use owo_colors::Stream::{Stderr, Stdout};
use print::{print_alt_mode_generic, print_cable, print_partner, print_pd, print_plug};
use yuca::{sysfs::*, watcher::*};

use crate::{
    print::{print_port, DeviceOpener, TreePrinter},
    styles::StyleApply,
};

#[derive(Debug, Clone, Bpaf)]
struct OptionsTree {
    /// Show any errors that occur while trying to read child devices or
    /// properties.
    show_errors: bool,
}

#[derive(Debug, Clone, Bpaf)]
#[bpaf(generate(options_watch_inner))]
struct OptionsWatch {
    /// Print the properties and unwatchable children of added and changed
    /// devices.
    show_devices: bool,
    /// Show any errors that occur while trying to read child devices or
    /// properties. (Only valid when --show-devices is given.)
    show_errors: bool,
}

fn options_watch() -> impl Parser<OptionsWatch> {
    options_watch_inner().guard(
        |opts| opts.show_devices || !opts.show_errors,
        "cannot use --show-errors without --show-devices",
    )
}

#[derive(Debug, Clone, Bpaf)]
enum OptionsCommand {
    #[bpaf(command)]
    /// Print a tree of all available Type-C devices.
    Tree(#[bpaf(external(options_tree))] OptionsTree),
    #[bpaf(command)]
    /// Watch for added, removed, and changed devices.
    Watch(#[bpaf(external(options_watch))] OptionsWatch),
}

#[derive(Debug, Clone, Bpaf)]
#[bpaf(options)]
struct Options {
    #[bpaf(external(options_command))]
    command: OptionsCommand,
}

fn do_list(opts: OptionsTree) {
    let mut p = TreePrinter::shallow_root().recursive();
    if opts.show_errors {
        p = p.show_errors();
    }

    let Ok(collection) = p.maybe_show_error(Port::collection()) else {
        return;
    };

    p.collection("Ports", collection, print_port);
}

enum EventKind {
    Added,
    Changed,
    Removed,
}

fn watch_device_path_stream<T: Device>(
    opts: OptionsWatch,
    kind: EventKind,
    stream: DevicePathStream<impl DevicePathParent, T::Path>,
    printer: fn(TreePrinter, DeviceOpener<'_, T>),
) -> impl Future<Output = ()> {
    stream
        .map(move |path| match path {
            Ok(path) => {
                let mut sys = Utf8PathBuf::from(SYS_CLASS_TYPEC);
                path.build_syspath(&mut sys);
                let (label, label_style) = match kind {
                    EventKind::Added => (
                        if opts.show_devices {
                            "Added"
                        } else {
                            // If not showing devices, then align this with the
                            // "Changed" and "Removed" labels.
                            "Added  "
                        },
                        styles::event_add(),
                    ),
                    EventKind::Changed => ("Changed", styles::event_change()),
                    EventKind::Removed => ("Removed", styles::event_remove()),
                };
                println!(
                    "{} {} {}",
                    label.style_if_supported(Stdout, label_style),
                    "::".style_if_supported(Stdout, styles::dim()),
                    sys.style_if_supported(Stdout, styles::label())
                );

                if opts.show_devices {
                    let mut p = TreePrinter::shallow_root().child();
                    if opts.show_errors {
                        p = p.show_errors();
                    }
                    printer(p, DeviceOpener::Path(path));
                }
            }
            Err(Overflowed { missed_events }) => {
                eprintln!(
                    "{} {}",
                    "Too slow!".style_if_supported(Stderr, styles::error_label()),
                    format_args!("Missed {missed_events} event(s).")
                        .style_if_supported(Stderr, styles::error_body())
                );
            }
        })
        .collect::<()>()
}

fn watch_everything_at_path<T: Device>(
    opts: OptionsWatch,
    w: Watcher,
    printer: fn(TreePrinter, DeviceOpener<'_, T>),
) -> WatchResult<impl Future<Output = ()>>
where
    T::Path: DevicePathWatchable,
{
    let added = watch_device_path_stream(
        opts.clone(),
        EventKind::Added,
        T::Path::any_added(&w)?,
        printer,
    );
    let changed = watch_device_path_stream(
        opts.clone(),
        EventKind::Changed,
        T::Path::any_changed(&w)?,
        printer,
    );
    let removed = watch_device_path_stream(
        opts.clone(),
        EventKind::Removed,
        T::Path::any_removed(&w)?,
        printer,
    );
    Ok(async {
        tokio::join!(added, changed, removed);
    })
}

async fn do_watch(opts: OptionsWatch) {
    let (w, jh) = Watcher::spawn_tokio(EventSource::Netlink).unwrap();

    // We ignore this task's return value, because if the dispatcher dies, we'll
    // know from the JoinHandle anyway.
    tokio::task::spawn(async move {
        Ok::<_, DispatcherDead>(tokio::join!(
            watch_everything_at_path(opts.clone(), w.clone(), print_port)?,
            watch_everything_at_path(opts.clone(), w.clone(), print_partner)?,
            watch_everything_at_path(opts.clone(), w.clone(), print_cable)?,
            watch_everything_at_path(opts.clone(), w.clone(), print_plug)?,
            watch_everything_at_path::<AltMode<PartnerPath>>(
                opts.clone(),
                w.clone(),
                print_alt_mode_generic
            )?,
            watch_everything_at_path::<AltMode<PlugPath>>(
                opts.clone(),
                w.clone(),
                print_alt_mode_generic
            )?,
            watch_everything_at_path(opts.clone(), w.clone(), print_pd)?,
        ))
    });

    println!(
        "{}",
        "Waiting for events...".style_if_supported(Stdout, styles::info())
    );

    match jh.await {
        Ok(Ok(())) => (),
        Ok(Err(err)) => eprintln!("Error from background watcher: {err:?}"),
        Err(err) => eprintln!("Error from background watcher: {err:?}"),
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let opts = options().run();

    match opts.command {
        OptionsCommand::Tree(opts) => do_list(opts),
        OptionsCommand::Watch(opts) => do_watch(opts).await,
    }
}
