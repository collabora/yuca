#!/bin/sh

# Only use 'umockdev-wrapper' if we're actually running the umockdev tests.
case "${1##*/}" in
  umockdev-*) exec umockdev-wrapper "$@" ;;
  *) exec "$@" ;;
esac
