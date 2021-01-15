//! A [Serde](https://serde.rs) implementation of the [Bebop
//! protocol](https://github.com/RainwayApp/bebop).

mod de;
mod error;
mod ser;

#[doc(inline)]
pub use de::{from_bytes, Deserializer};
#[doc(inline)]
pub use error::{Error, Result};
#[doc(inline)]
pub use ser::{to_bytes, to_bytes_message, Serializer};
