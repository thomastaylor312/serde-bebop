use std::fmt::Display;

use serde::{de, ser};
use thiserror::Error as ThisError;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Debug, PartialEq, ThisError)]
pub enum Error {
    // One or more variants that can be created by data structures through the
    // `ser::Error` and `de::Error` traits. For example the Serialize impl for
    // Mutex<T> might return an error because the mutex is poisoned, or the
    // Deserialize impl for a struct may return an error because a required
    // field is missing.
    #[error("{0}")]
    Message(String),

    #[error("Unexpected end of data")]
    Eof,
    #[error("Expected to get array length")]
    ExpectedArrayLength,
    #[error("Expected to get map length")]
    ExpectedMapLength,
    #[error("Enum variants cannot contain data in Bebop")]
    VariantDataNotAllowed,
    #[error("i8 serialization is not supported in Bebop")]
    Int8NotSupported,
    #[error("Encountered extra bytes at end of data after parsing")]
    TrailingBytes,
    #[error("Expected to serialize {0} struct members, got {1}")]
    StructLengthMismatch(usize, usize),
}

impl ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}
