#!/bin/sh

set -eu

if [ -n "${UMOCKDEV_RUN_STAGE2:-}" ]; then
  # umockdev creates typec devices under /sys/bus, so symlink it to the expected
  # path.
  ln -sf "$UMOCKDEV_DIR/sys/bus/typec/devices" "$UMOCKDEV_DIR/sys/class/typec"
  exec cargo run -p yuca-cli --features yuca/rustix-use-libc -- "$@"
fi

export UMOCKDEV_RUN_STAGE2=1
umockdev-run \
  -d "$(dirname "$0")/../umockdev/devices-${UMOCKDEV_DEVICES:-default}.umockdev" \
  -- sh "$0" "$@"
