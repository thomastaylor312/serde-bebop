use std::{convert::TryInto, mem::size_of};

use serde::de::{
    self, DeserializeSeed, EnumAccess, IntoDeserializer, MapAccess, SeqAccess, VariantAccess,
    Visitor,
};
use serde::Deserialize;

use crate::error::{Error, Result};

const BEBOP_STARTING_INDEX: usize = 1;

pub struct Deserializer<'de> {
    // This string starts with the input data and characters are truncated off
    // the beginning as data is parsed.
    input: &'de [u8],
    skipped_index: bool,
    is_message: bool,
}

impl<'de> Deserializer<'de> {
    pub fn from_bytes(input: &'de [u8]) -> Self {
        Deserializer {
            input,
            skipped_index: false,
            is_message: false,
        }
    }
}

/// Deserializes from raw bytes to the given type
pub fn from_bytes<'a, T>(s: &'a [u8]) -> Result<T>
where
    T: Deserialize<'a>,
{
    let mut deserializer = Deserializer::from_bytes(s);
    let t = T::deserialize(&mut deserializer)?;
    if deserializer.input.is_empty() {
        Ok(t)
    } else {
        Err(Error::TrailingBytes)
    }
}

impl<'de> Deserializer<'de> {
    fn parse_string(&mut self) -> Result<&'de str> {
        // First, let's get the length of the string
        let str_len = self.parse_object_size()?;

        // Now that we have the string length, split again
        if str_len > self.input.len() {
            return Err(Error::Eof);
        }
        let (data, remaining) = self.input.split_at(str_len);
        self.input = remaining;

        // Validate that the string is actually utf-8
        Ok(std::str::from_utf8(data).map_err(|_| Error::InvalidUtf8)?)
    }

    /// grabs the size of the current object in the input, advances the input to
    /// the start of the object, and returns the size
    fn parse_object_size(&mut self) -> Result<usize> {
        let size = size_of::<u32>();
        // Before we split, check remaining output to avoid panic
        if size > self.input.len() {
            return Err(Error::Eof);
        }
        let (raw, remaining) = self.input.split_at(size);
        self.input = remaining;
        Ok(u32::from_le_bytes(raw.try_into().map_err(|_| Error::InvalidNumberBytes)?) as usize)
    }
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    fn is_human_readable(&self) -> bool {
        false
    }

    // Look at the input data to decide what Serde data model type to
    // deserialize as. Not all data formats are able to support this operation.
    // Formats that support `deserialize_any` are known as self-describing.
    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        Err(Error::Message(
            "Bebop does not support deserializer_any".to_string(),
        ))
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // Grab the single byte representing the bool
        let byte = self.input.first().ok_or(Error::Eof)?;
        let val = if *byte == 0u8 {
            false
        } else if *byte == 1u8 {
            true
        } else {
            return Err(Error::InvalidBool);
        };
        // Advance the input
        self.input = &self.input[1..];
        visitor.visit_bool(val)
    }

    // The `parse_signed` function is generic over the integer type `T` so here
    // it is invoked with `T=i8`. The next 8 methods are similar.
    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let size = size_of::<i8>();
        // Before we split, check remaining output to avoid panic
        if size > self.input.len() {
            return Err(Error::Eof);
        }
        let (raw, remaining) = self.input.split_at(size);
        self.input = remaining;
        visitor.visit_i8(i8::from_le_bytes(
            raw.try_into().map_err(|_| Error::InvalidNumberBytes)?,
        ))
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let size = size_of::<i16>();
        // Before we split, check remaining output to avoid panic
        if size > self.input.len() {
            return Err(Error::Eof);
        }
        let (raw, remaining) = self.input.split_at(size);
        self.input = remaining;
        visitor.visit_i16(i16::from_le_bytes(
            raw.try_into().map_err(|_| Error::InvalidNumberBytes)?,
        ))
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let size = size_of::<i32>();
        // Before we split, check remaining output to avoid panic
        if size > self.input.len() {
            return Err(Error::Eof);
        }
        let (raw, remaining) = self.input.split_at(size);
        self.input = remaining;
        visitor.visit_i32(i32::from_le_bytes(
            raw.try_into().map_err(|_| Error::InvalidNumberBytes)?,
        ))
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let size = size_of::<i64>();
        // Before we split, check remaining output to avoid panic
        if size > self.input.len() {
            return Err(Error::Eof);
        }
        let (raw, remaining) = self.input.split_at(size);
        self.input = remaining;
        visitor.visit_i64(i64::from_le_bytes(
            raw.try_into().map_err(|_| Error::InvalidNumberBytes)?,
        ))
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // This is easier than the others because we just pop a byte off of the input
        let byte = self.input.first().ok_or(Error::Eof)?;
        self.input = &self.input[1..];
        visitor.visit_u8(*byte)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let size = size_of::<u16>();
        // Before we split, check remaining output to avoid panic
        if size > self.input.len() {
            return Err(Error::Eof);
        }
        let (raw, remaining) = self.input.split_at(size);
        self.input = remaining;
        visitor.visit_u16(u16::from_le_bytes(
            raw.try_into().map_err(|_| Error::InvalidNumberBytes)?,
        ))
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let size = size_of::<u32>();
        // Before we split, check remaining output to avoid panic
        if size > self.input.len() {
            return Err(Error::Eof);
        }
        let (raw, remaining) = self.input.split_at(size);
        self.input = remaining;
        visitor.visit_u32(u32::from_le_bytes(
            raw.try_into().map_err(|_| Error::InvalidNumberBytes)?,
        ))
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let size = size_of::<u64>();
        // Before we split, check remaining output to avoid panic
        if size > self.input.len() {
            return Err(Error::Eof);
        }
        let (raw, remaining) = self.input.split_at(size);
        self.input = remaining;
        visitor.visit_u64(u64::from_le_bytes(
            raw.try_into().map_err(|_| Error::InvalidNumberBytes)?,
        ))
    }

    // Float parsing is stupidly hard.
    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let size = size_of::<f32>();
        // Before we split, check remaining output to avoid panic
        if size > self.input.len() {
            return Err(Error::Eof);
        }
        let (raw, remaining) = self.input.split_at(size);
        self.input = remaining;
        visitor.visit_f32(f32::from_le_bytes(
            raw.try_into().map_err(|_| Error::InvalidNumberBytes)?,
        ))
    }

    // Float parsing is stupidly hard.
    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let size = size_of::<f64>();
        // Before we split, check remaining output to avoid panic
        if size > self.input.len() {
            return Err(Error::Eof);
        }
        let (raw, remaining) = self.input.split_at(size);
        self.input = remaining;
        visitor.visit_f64(f64::from_le_bytes(
            raw.try_into().map_err(|_| Error::InvalidNumberBytes)?,
        ))
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // Chars are serialized as strings, so deserialize it and check its length
        let data = self.parse_string()?;
        if data.chars().count() != 1 {
            return Err(Error::InvalidChar);
        }
        // We just checked length, so unwrapping is ok
        visitor.visit_char(data.chars().next().unwrap())
    }

    // Refer to the "Understanding deserializer lifetimes" page for information
    // about the three deserialization flavors of strings in Serde.
    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.parse_string()?)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_string(self.parse_string()?.to_owned())
    }

    // The `Serializer` implementation on the previous page serialized byte
    // arrays as JSON arrays of bytes. Handle that representation here.
    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // Byte arrays can be read directly off the input after getting the length
        let len = self.parse_object_size()?;

        // Now grab the data
        if len > self.input.len() {
            return Err(Error::Eof);
        }
        let (data, remaining) = self.input.split_at(len);
        self.input = remaining;
        visitor.visit_bytes(data)
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // Byte arrays can be read directly off the input after getting the length
        let len = self.parse_object_size()?;

        // Now grab the data
        if len > self.input.len() {
            return Err(Error::Eof);
        }
        let (data, remaining) = self.input.split_at(len);
        self.input = remaining;
        visitor.visit_byte_buf(data.to_owned())
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        if self.is_message && self.skipped_index {
            // We are handling a message, so check for the index
            visitor.visit_none()
        } else if self.is_message && !self.skipped_index {
            visitor.visit_some(self)
        } else {
            // We are handling a struct, so the data is present. Just return a visit_some
            visitor.visit_some(self)
        }
    }

    // In Serde, unit means an anonymous value containing no data.
    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // A unit in a struct means that we should always be missing a field in
        // a message. A Bebop struct cannot contain it
        if self.is_message && self.skipped_index {
            // A unit type means there is a missing index, so return an error if there is data
            Err(Error::UnexpectedData)
        } else if self.is_message && !self.skipped_index {
            visitor.visit_unit()
        } else {
            // Structs cannot contain empty
            Err(Error::InvalidUnit)
        }
    }

    // Unit struct means a named value containing no data.
    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    // As is done here, serializers are encouraged to treat newtype structs as
    // insignificant wrappers around the data they contain. That means not
    // parsing anything other than the contained value.
    fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    // Deserialization of compound types like sequences and maps happens by
    // passing the visitor an "Access" object that gives it the ability to
    // iterate through the data contained in the sequence.
    fn deserialize_seq<V>(mut self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let len = self.parse_object_size()?;
        visitor.visit_seq(List::new(&mut self, len))
    }

    // We treat tuples like arrays
    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    // Tuple structs are also like arrays
    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(mut self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let len = self.parse_object_size()?;
        visitor.visit_map(List::new(&mut self, len))
    }

    fn deserialize_struct<V>(
        mut self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // Parse the first 4 bytes as a u32. If that length matches the length
        // of the rest of the body, it is a message. Otherwise, assume it is a
        // Bebop struct and handle accordingly
        let size = size_of::<u32>();
        // Before we split, check remaining output to avoid panic
        if size > self.input.len() {
            return Err(Error::Eof);
        }
        // Just peek the bytes, don't consume them yet
        let bytes = &self.input[..size];
        let len =
            u32::from_le_bytes(bytes.try_into().map_err(|_| Error::InvalidNumberBytes)?) as usize;
        // TODO: Figure out how to pull out a nested message: Probably assume it is the length and check that the last byte at the end of that range is a 0 byte
        let next_index = if self.input[size..].len() == len {
            println!("got message with name {}", _name);
            // Consume the bytes we used for the message length and trim off the trailing 0 byte
            self.input = &self.input[size..];
            let (_, rest) = self.input.split_last().ok_or(Error::Eof)?;
            self.input = rest;
            self.is_message = true;
            // If it is a message, make sure to set the message index (starting with 1)
            Some(BEBOP_STARTING_INDEX)
        } else {
            println!("got struct with name {}", _name);
            None
        };
        visitor.visit_seq(StructAccess::new(&mut self, next_index))
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_enum(self)
    }

    // An identifier in Serde is the type that identifies a field of a struct or
    // the variant of an enum. In JSON, struct fields and enum variants are
    // represented as strings. In other formats they may be represented as
    // numeric indices.
    fn deserialize_identifier<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        Err(Error::Message(
            "Bebop does not support deserialize identifier".to_string(),
        ))
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }
}

struct List<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
    expected_len: usize,
    current_len: usize,
}

impl<'a, 'de> List<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>, expected_len: usize) -> Self {
        List {
            de,
            expected_len,
            current_len: 0,
        }
    }
}

impl<'de, 'a> SeqAccess<'de> for List<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        // If we have serialized all entries, we are done
        if self.current_len == self.expected_len {
            Ok(None)
        } else {
            // Otherwise, increment the current and deserialize the next message
            self.current_len += 1;
            seed.deserialize(&mut *self.de).map(Some)
        }
    }
}

impl<'de, 'a> MapAccess<'de> for List<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        // If we have serialized all entries, we are done
        if self.current_len == self.expected_len {
            Ok(None)
        } else {
            // Otherwise, increment the current and deserialize the next message
            self.current_len += 1;
            seed.deserialize(&mut *self.de).map(Some)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        // We handle the index incrementing in the key seed, so just serialize
        seed.deserialize(&mut *self.de)
    }
}

struct StructAccess<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
    next_index: Option<usize>,
}

impl<'a, 'de> StructAccess<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>, next_index: Option<usize>) -> Self {
        StructAccess { de, next_index }
    }
}

impl<'de, 'a> SeqAccess<'de> for StructAccess<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        // If we get to the end of the data we are done
        if self.de.input.is_empty() {
            Ok(None)
        } else {
            // Otherwise, increment the current index (if a message) and trim the index off the input if it is a message
            if let Some(i) = self.next_index.take() {
                println!("Expected index: {}", i);
                println!("Current input: {:?}", self.de.input);
                let possible_index = *self.de.input.first().ok_or(Error::Eof)? as usize;
                // Either way, the index increments, so we know that this one
                // wasn't present in the message (or was handled)
                self.next_index = Some(i + 1);
                if i == possible_index {
                    // Consume the index so the next part parses properly
                    self.de.input = &self.de.input[1..];
                    // Let the deserializer know that this index wasn't skipped
                    self.de.skipped_index = false;
                } else {
                    // Let the deserializer that this one was skipped
                    self.de.skipped_index = true;
                }
            }
            seed.deserialize(&mut *self.de).map(Some)
        }
    }
}

impl<'a, 'de> EnumAccess<'de> for &'a mut Deserializer<'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: DeserializeSeed<'de>,
    {
        // All we need is a u32, so get that data and deserialize
        let index = self.parse_object_size()?;
        let val = seed.deserialize(index.into_deserializer())?;
        // Parse the colon separating map key from value.
        Ok((val, self))
    }
}

impl<'a, 'de> VariantAccess<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    // If the `Visitor` expected this variant to be a unit variant, the input
    // should have been the plain string case handled in `deserialize_enum`.
    fn unit_variant(self) -> Result<()> {
        // We already serialized the index, so just return ok
        Ok(())
    }

    // The rest of these all return errors as they are not supported in bebop
    fn newtype_variant_seed<T>(self, _seed: T) -> Result<T::Value>
    where
        T: DeserializeSeed<'de>,
    {
        Err(Error::VariantDataNotAllowed)
    }

    fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        Err(Error::VariantDataNotAllowed)
    }

    fn struct_variant<V>(self, _fields: &'static [&'static str], _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        Err(Error::VariantDataNotAllowed)
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use super::*;
    use serde::Deserialize;

    #[derive(Debug, Deserialize, PartialEq)]
    struct SimpleStruct {
        name: String,
        age: u16,
    }

    #[test]
    fn test_valid_struct() {
        // Taken from one of the bebop reference implementations
        let data: Vec<u8> = vec![7, 0, 0, 0, 67, 104, 97, 114, 108, 105, 101, 28, 0];
        let deserialized: SimpleStruct = from_bytes(&data).expect("Unable to deserialize");
        let expected = SimpleStruct {
            name: "Charlie".to_string(),
            age: 28,
        };
        assert_eq!(deserialized, expected);
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct SimpleMessage {
        name: String,
        age: Option<u16>,
    }

    #[test]
    fn test_valid_message_all_fields() {
        // Taken from one of the bebop reference implementations
        let data: Vec<u8> = vec![
            16, 0, 0, 0, 1, 7, 0, 0, 0, 67, 104, 97, 114, 108, 105, 101, 2, 28, 0, 0,
        ];

        let deserialized: SimpleMessage = from_bytes(&data).expect("Unable to deserialize");
        let expected = SimpleMessage {
            name: "Charlie".to_string(),
            age: Some(28),
        };

        assert_eq!(deserialized, expected);
    }

    #[test]
    fn test_valid_message_some_fields() {
        // Taken from one of the bebop reference implementations
        let data: Vec<u8> = vec![
            13, 0, 0, 0, 1, 7, 0, 0, 0, 67, 104, 97, 114, 108, 105, 101, 0,
        ];

        let deserialized: SimpleMessage = from_bytes(&data).expect("Unable to deserialize");
        let expected = SimpleMessage {
            name: "Charlie".to_string(),
            age: None,
        };

        assert_eq!(deserialized, expected);
    }

    #[derive(Debug, Deserialize, PartialEq)]
    #[allow(dead_code)]
    enum Fun {
        Not,
        Somewhat,
        Really,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct Complex {
        name: Option<String>,
        fun_level: Fun,
        map: HashMap<String, SimpleStruct>,
        message_map: HashMap<String, SimpleMessage>,
        list: Vec<f32>,
        boolean: bool,
        int16: i16,
        int32: i32,
        int64: i64,
        uint16: u16,
        uint32: u32,
        uint64: u64,
        byte: u8,
        float64: f64,
    }

    #[test]
    fn test_complex() {
        // Taken from one of the bebop reference implementations
        let data: Vec<u8> = vec![
            124, 0, 0, 0, 1, 7, 0, 0, 0, 67, 104, 97, 114, 108, 105, 101, 2, 1, 0, 0, 0, 3, 1, 0,
            0, 0, 3, 0, 0, 0, 111, 110, 101, 3, 0, 0, 0, 79, 110, 101, 16, 0, 4, 1, 0, 0, 0, 3, 0,
            0, 0, 111, 110, 101, 9, 0, 0, 0, 1, 3, 0, 0, 0, 79, 110, 101, 0, 5, 2, 0, 0, 0, 218,
            15, 73, 64, 77, 248, 45, 64, 6, 1, 7, 253, 255, 8, 42, 0, 0, 0, 9, 21, 205, 91, 7, 0,
            0, 0, 0, 10, 3, 0, 11, 42, 0, 0, 0, 12, 21, 205, 91, 7, 0, 0, 0, 0, 13, 17, 14, 74,
            216, 18, 77, 251, 33, 9, 64, 0,
        ];

        let deserialized: Complex = from_bytes(&data).expect("Unable to deserialize");

        let mut map = HashMap::new();
        map.insert(
            "one".to_string(),
            SimpleStruct {
                name: "One".to_string(),
                age: 16,
            },
        );
        let mut message_map = HashMap::new();
        message_map.insert(
            "one".to_string(),
            SimpleMessage {
                name: "One".to_string(),
                age: None,
            },
        );
        let expected = Complex {
            name: Some("Charlie".to_string()),
            fun_level: Fun::Somewhat,
            map,
            message_map,
            list: vec![3.1415926, 2.71828],
            boolean: true,
            int16: -3,
            int32: 42,
            int64: 123456789,
            uint16: 3,
            uint32: 42,
            uint64: 123456789,
            byte: 17,
            float64: 3.1415926,
        };
        assert_eq!(deserialized, expected);
    }
}
