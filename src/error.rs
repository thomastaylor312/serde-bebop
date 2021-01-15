use std::fmt::Display;

use serde::{de, ser};
use thiserror::Error as ThisError;

/// A custom shorthand result type that always has an error type of [`Error`](Error)
pub type Result<T> = std::result::Result<T, Error>;

/// The possible errors that the Bebop serializer and deserializer can return
#[derive(Clone, Debug, PartialEq, ThisError)]
pub enum Error {
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
    #[error("Found byte that does not parse to a bool")]
    InvalidBool,
    #[error("Got invalid number of bytes when parsing a numeric value")]
    InvalidNumberBytes,
    #[error("Got invalid UTF-8 data when parsing a string")]
    InvalidUtf8,
    #[error("Expected a string with an exact length of 1 to deserialize as a char")]
    InvalidChar,
    #[error("Unable to deserialize message. Missing `Option` in struct")]
    InvalidStruct,
    #[error("Unable to deserialze a Bebop struct to a Rust struct containing an option. It must be a Bebop message")]
    InvalidUnit,
    #[error("Got data when none was expected")]
    UnexpectedData,
    #[error("Invalid message terminator byte")]
    InvalidMessageTerminator,
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
