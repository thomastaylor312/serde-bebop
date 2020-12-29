use std::ops::{AddAssign, MulAssign, Neg};
use std::{convert::TryInto, mem::size_of};

use serde::de::{
    self, DeserializeSeed, EnumAccess, IntoDeserializer, MapAccess, SeqAccess, VariantAccess,
    Visitor,
};
use serde::Deserialize;

use crate::error::{Error, Result};

pub struct Deserializer<'de> {
    // This string starts with the input data and characters are truncated off
    // the beginning as data is parsed.
    input: &'de [u8],
    next_index: Option<usize>,
}

impl<'de> Deserializer<'de> {
    pub fn from_bytes(input: &'de [u8]) -> Self {
        Deserializer {
            input,
            next_index: None,
        }
    }
}

// By convention, the public API of a Serde deserializer is one or more
// `from_xyz` methods such as `from_str`, `from_bytes`, or `from_reader`
// depending on what Rust types the deserializer is able to consume as input.
//
// This basic deserializer supports only `from_str`.
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

// SERDE IS NOT A PARSING LIBRARY. This impl block defines a few basic parsing
// functions from scratch. More complicated formats may wish to use a dedicated
// parsing library to help implement their Serde deserializer.
impl<'de> Deserializer<'de> {
    // Look at the first character in the input without consuming it.
    fn peek_char(&mut self) -> Result<char> {
        todo!()
    }

    // Consume the first character in the input.
    fn next_char(&mut self) -> Result<char> {
        let ch = self.peek_char()?;
        self.input = &self.input[ch.len_utf8()..];
        Ok(ch)
    }

    // Parse the JSON identifier `true` or `false`.
    fn parse_bool(&mut self) -> Result<bool> {
        Ok(true)
    }

    // Parse a group of decimal digits as an unsigned integer of type T.
    //
    // This implementation is a bit too lenient, for example `001` is not
    // allowed in JSON. Also the various arithmetic operations can overflow and
    // panic or return bogus data. But it is good enough for example code!
    fn parse_unsigned<T>(&mut self) -> Result<T>
    where
        T: AddAssign<T> + MulAssign<T> + From<u8>,
    {
        todo!()
    }

    // Parse a possible minus sign followed by a group of decimal digits as a
    // signed integer of type T.
    fn parse_signed<T>(&mut self) -> Result<T>
    where
        T: Neg<Output = T> + AddAssign<T> + MulAssign<T> + From<i8>,
    {
        // Optional minus sign, delegate to `parse_unsigned`, negate if negative.
        unimplemented!()
    }

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
        unimplemented!()
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
        match self.next_index.take() {
            // We are handling a message, so check for the index
            Some(index) => {
                let possible_index = *self.input.first().ok_or(Error::Eof)? as usize;
                // Either way, the index increments, so we know that this one
                // wasn't present in the message (or was handled)
                self.next_index = Some(index + 1);
                if index == possible_index {
                    // Consume the index so the next part parses properly
                    self.input = &self.input[1..];
                    visitor.visit_some(self)
                } else {
                    visitor.visit_none()
                }
            }
            // We are handling a struct, so the data is present. Just return a visit_some
            None => visitor.visit_some(self),
        }
    }

    // In Serde, unit means an anonymous value containing no data.
    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // A unit in a struct means that we should always be missing a field in
        // a message. A Bebop struct cannot contain it
        match self.next_index.take() {
            // We are handling a message, so check for the index
            Some(index) => {
                let possible_index = *self.input.first().ok_or(Error::Eof)? as usize;
                // Either way, the index increments, so we know that this one
                // wasn't present in the message (or was handled)
                self.next_index = Some(index + 1);
                if index == possible_index {
                    // A unit type means there is a missing index, so return an error
                    return Err(Error::UnexpectedData);
                } else {
                    visitor.visit_unit()
                }
            }
            // We are handling a struct, so the data is present. Just return a visit_some
            None => return Err(Error::InvalidUnit),
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
        visitor.visit_seq(List::new(&mut self, len))
    }

    // Structs look just like maps in JSON.
    //
    // Notice the `fields` parameter - a "struct" in the Serde data model means
    // that the `Deserialize` implementation is required to know what the fields
    // are before even looking at the input data. Any key-value pairing in which
    // the fields cannot be known ahead of time is probably a map.
    fn deserialize_struct<V>(
        self,
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
        // If it is a message, make sure to set the message index (starting with 1)
        self.deserialize_map(visitor)
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
    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
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
        Ok(val, self)
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
    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: DeserializeSeed<'de>,
    {
        Err(Error::VariantDataNotAllowed)
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        Err(Error::VariantDataNotAllowed)
    }

    fn struct_variant<V>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        Err(Error::VariantDataNotAllowed)
    }
}
