#!/bin/sh

# There are "devices" that have no subsystem attached, but we still want to be
# able to write tests that use them. If they just get added to the umockdev
# file as-is, umockdev *crashes*, so this awk script gets used to fill in a
# stub subsystem value for those devices.
read -r -d '' FILL_MISSING_SUBSYSTEMS <<'EOF' || :
/^P:/ {
  path=$2
  gsub(/\//, "_", path)
  has_subsystem=0
}
/^E: SUBSYSTEM/ { has_subsystem=1 }
/^A:/ {
  if (!has_subsystem) {
    print "E: SUBSYSTEM=typecfake" path
    has_subsystem=1
  }
}

{ print }
EOF

find /sys/class/typec/*/ -type d -exec test -e {}/uevent \; -exec umockdev-record {} + \
  | awk "$FILL_MISSING_SUBSYSTEMS"
