pub mod sysfs;
pub mod types;

use std::num::ParseIntError;

use rustix::io::Errno;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("I/O: {0}")]
    Io(#[from] std::io::Error),
    #[error("parse error")]
    Parse,
    #[error("identity attribute unavailable")]
    IdentityUnavailable,
}

impl From<Errno> for Error {
    fn from(value: Errno) -> Self {
        Error::Io(value.into())
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
