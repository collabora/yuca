#![cfg_attr(docsrs, feature(doc_cfg))]
#![doc = include_str!("../README.md")]
#![doc = include_str!("../docs/overview.md")]

pub mod sysfs;
pub mod types;
pub mod watcher;

use std::num::ParseIntError;

use rustix::io::Errno;
use thiserror::Error;

#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    #[error("I/O: {0}")]
    Io(std::io::ErrorKind),
    #[error("parse error")]
    Parse,
    #[error("identity attribute unavailable")]
    IdentityUnavailable,
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Error::Io(value.kind())
    }
}

impl From<Errno> for Error {
    fn from(value: Errno) -> Self {
        std::io::Error::from(value).into()
    }
}

impl From<nix::errno::Errno> for Error {
    fn from(value: nix::errno::Errno) -> Self {
        Errno::from_raw_os_error(value as i32).into()
    }
}

impl From<strum::ParseError> for Error {
    fn from(_: strum::ParseError) -> Self {
        Error::Parse
    }
}

impl From<ParseIntError> for Error {
    fn from(_: ParseIntError) -> Self {
        Error::Parse
    }
}

pub type Result<T> = std::result::Result<T, Error>;
