use serde::{ser, Serialize};

use crate::error::{Error, Result};

pub struct Serializer {
    // This buffer starts empty and we append bytes to it
    output: Vec<u8>,
    is_message: bool,
    has_option: bool,
}

pub fn to_bytes<T>(value: &T) -> Result<Vec<u8>>
where
    T: Serialize,
{
    let mut serializer = Serializer {
        output: Vec::new(),
        is_message: false,
        has_option: false,
    };
    value.serialize(&mut serializer)?;
    Ok(serializer.output)
}

/// Forces serialization as a message. Normally, the serializer will
/// automatically assume a Bebop message if there are optional fields and a
/// Bebop struct if all the fields are required. This will force encoding as a
/// message. Right now this will only force the top level struct to be a
/// message, any embedded structs will not be assumed
// TODO: Should we add a parameter to recurse all the way down? Specifically,
// should we only encode the top level struct as a message unless recurse: true
// is specified?
pub fn to_bytes_message<T>(value: &T) -> Result<Vec<u8>>
where
    T: Serialize,
{
    let mut serializer = Serializer {
        output: Vec::new(),
        is_message: true,
        has_option: false,
    };
    value.serialize(&mut serializer)?;
    Ok(serializer.output)
}

// TODO: Maybe add a `to_writer` and `to_async_writer`

// TODO: What would supporting the GUID and date types from bebop look like?

impl<'a> ser::Serializer for &'a mut Serializer {
    type Ok = ();

    type Error = Error;

    type SerializeSeq = Self;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = Self;
    type SerializeStruct = StructSerializer<'a>;
    type SerializeStructVariant = StructSerializer<'a>;

    fn serialize_bool(self, v: bool) -> Result<()> {
        self.output.push(if v { 0x01 } else { 0x00 });
        Ok(())
    }

    fn serialize_i8(self, _v: i8) -> Result<()> {
        Err(Error::Int8NotSupported)
    }

    fn serialize_i16(self, v: i16) -> Result<()> {
        self.output.extend(&v.to_le_bytes());
        Ok(())
    }

    fn serialize_i32(self, v: i32) -> Result<()> {
        self.output.extend(&v.to_le_bytes());
        Ok(())
    }

    fn serialize_i64(self, v: i64) -> Result<()> {
        self.output.extend(&v.to_le_bytes());
        Ok(())
    }

    fn serialize_u8(self, v: u8) -> Result<()> {
        self.output.extend(&v.to_le_bytes());
        Ok(())
    }

    fn serialize_u16(self, v: u16) -> Result<()> {
        self.output.extend(&v.to_le_bytes());
        Ok(())
    }

    fn serialize_u32(self, v: u32) -> Result<()> {
        self.output.extend(&v.to_le_bytes());
        Ok(())
    }

    fn serialize_u64(self, v: u64) -> Result<()> {
        self.output.extend(&v.to_le_bytes());
        Ok(())
    }

    fn serialize_f32(self, v: f32) -> Result<()> {
        self.output.extend(&v.to_le_bytes());
        Ok(())
    }

    fn serialize_f64(self, v: f64) -> Result<()> {
        self.output.extend(&v.to_le_bytes());
        Ok(())
    }

    // Serialize a char as a single-character string.
    fn serialize_char(self, v: char) -> Result<()> {
        self.output.extend(&(v.len_utf8() as u32).to_le_bytes());
        let mut encoded = Vec::with_capacity(v.len_utf8());
        v.encode_utf8(&mut encoded);
        self.output.extend(encoded);
        Ok(())
    }

    fn serialize_str(self, v: &str) -> Result<()> {
        self.output
            .extend(&(v.as_bytes().len() as u32).to_le_bytes());
        self.output.extend(v.as_bytes());
        Ok(())
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<()> {
        use serde::ser::SerializeSeq;
        let mut seq = self.serialize_seq(Some(v.len()))?;
        for byte in v {
            seq.serialize_element(byte)?;
        }
        seq.end()
    }

    fn serialize_none(self) -> Result<()> {
        // Mark that we have an optional
        self.has_option = true;
        Ok(())
    }

    fn serialize_some<T>(self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        // Mark that we have an optional
        self.has_option = true;
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<()> {
        Ok(())
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<()> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        variant_index: u32,
        _variant: &'static str,
    ) -> Result<()> {
        self.output.extend(&variant_index.to_le_bytes());
        Ok(())
    }

    // Serializers are encouraged to treat newtype structs as
    // insignificant wrappers around the data they contain.
    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        // This cannot be supported as enums in bebop cannot contain additional data
        Err(Error::VariantDataNotAllowed)
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
        self.output
            .extend(&(len.ok_or(Error::ExpectedArrayLength)? as u32).to_le_bytes());
        Ok(self)
    }

    // We'll just treat a tuple as an array
    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        self.serialize_seq(Some(len))
    }

    // Tuple structs look just like sequences in Bebop
    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        // This cannot be supported as enums in bebop cannot contain additional data
        Err(Error::VariantDataNotAllowed)
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap> {
        // Add the length of the map
        self.output
            .extend(&(len.ok_or(Error::ExpectedMapLength)? as u32).to_le_bytes());
        Ok(self)
    }

    fn serialize_struct(self, _name: &'static str, len: usize) -> Result<Self::SerializeStruct> {
        // Set up our custom serializer
        Ok(StructSerializer {
            parts: Vec::new(),
            expected_length: len,
            has_option: false,
            inner: self,
        })
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        // This cannot be supported as enums in bebop cannot contain additional data
        Err(Error::VariantDataNotAllowed)
    }
}

pub struct StructSerializer<'a> {
    // Because bebop structs require all fields to be set, we need to track for
    // `Option` values so we can represent as a message or a struct
    parts: Vec<Option<Vec<u8>>>,
    expected_length: usize,
    has_option: bool,
    inner: &'a mut Serializer,
}

impl<'a> StructSerializer<'a> {
    // Messages (i.e. things that can be optional) are serialized differently
    fn serialize_message(self) -> Result<()> {
        // Create the body of the message (so we can get length), we are using
        // zip so we have the right index type (and starting index) for
        // serialization
        let mut body = (1u8..)
            .zip(self.parts)
            .fold(Vec::new(), |mut acc, (index, data)| {
                // If there is data push the index and then extend with the body
                if let Some(bytes) = data {
                    acc.push(index);
                    acc.extend(bytes);
                }
                acc
            });
        // Push the final empty terminator byte
        body.push(0u8);

        // Now we can get a length and add that to the actual serializer
        self.inner.output.extend(&(body.len() as u32).to_le_bytes());
        self.inner.output.extend(body);
        Ok(())
    }

    // Structs are a simple concat of all the data members
    fn serialize_struct(self) -> Result<()> {
        // This is only called internally, so we aren't handling the case where
        // a None exists in the Vec
        self.inner
            .output
            .extend(self.parts.into_iter().flatten().flatten());
        Ok(())
    }

    // These are the same functions needed for the trait implementation so they
    // can be reused
    fn serialize_field_inner<T>(&mut self, _key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        let mut serializer = Serializer {
            output: Vec::new(),
            is_message: false,
            has_option: false,
        };
        value.serialize(&mut serializer)?;

        // Set that we have an optional if needed
        self.has_option = serializer.has_option || self.has_option;

        // Not sure if there is a better way to check, but basically, if it is
        // none, it will still be empty
        if serializer.output.is_empty() {
            self.parts.push(None)
        } else {
            self.parts.push(Some(serializer.output))
        }
        Ok(())
    }

    fn end_inner(self) -> Result<()> {
        // Validate that we are serializing the right amount of things
        if self.expected_length != self.parts.len() {
            return Err(Error::StructLengthMismatch(
                self.expected_length,
                self.parts.len(),
            ));
        }
        // If we are forcing message encoding or we have an option, encode as a message
        if self.has_option || self.inner.is_message {
            self.serialize_message()
        } else {
            self.serialize_struct()
        }
    }
}

impl<'a> ser::SerializeStruct for StructSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.serialize_field_inner(key, value)
    }

    fn end(self) -> Result<()> {
        self.end_inner()
    }
}

impl<'a> ser::SerializeSeq for &'a mut Serializer {
    // Must match the `Ok` type of the serializer.
    type Ok = ();
    // Must match the `Error` type of the serializer.
    type Error = Error;

    // Serialize a single element of the sequence.
    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut **self)
    }

    // Close the sequence.
    fn end(self) -> Result<()> {
        Ok(())
    }
}

impl<'a> ser::SerializeTuple for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        Ok(())
    }
}

impl<'a> ser::SerializeTupleStruct for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        Ok(())
    }
}

impl<'a> ser::SerializeTupleVariant for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        Ok(())
    }
}

impl<'a> ser::SerializeMap for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        key.serialize(&mut **self)
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        Ok(())
    }
}

impl<'a> ser::SerializeStructVariant for StructSerializer<'a> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.serialize_field_inner(key, value)
    }

    fn end(self) -> Result<()> {
        self.end_inner()
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use super::*;
    use serde::Serialize;

    #[derive(Serialize)]
    struct SimpleStruct {
        name: String,
        age: u16,
    }

    #[test]
    fn test_valid_struct() {
        let data = SimpleStruct {
            name: "Charlie".to_string(),
            age: 28,
        };

        let raw = to_bytes(&data).expect("Unable to serialize");
        // Taken from one of the bebop reference implementations
        let expected: Vec<u8> = vec![7, 0, 0, 0, 67, 104, 97, 114, 108, 105, 101, 28, 0];
        assert_eq!(raw, expected);
    }

    #[test]
    fn test_valid_struct_message() {
        let data = SimpleStruct {
            name: "Charlie".to_string(),
            age: 28,
        };

        // Make sure if we force encoding as a message that it works
        let raw = to_bytes_message(&data).expect("Unable to serialize");
        // Taken from one of the bebop reference implementations
        let expected: Vec<u8> = vec![
            16, 0, 0, 0, 1, 7, 0, 0, 0, 67, 104, 97, 114, 108, 105, 101, 2, 28, 0, 0,
        ];
        assert_eq!(raw, expected);
    }

    #[derive(Serialize)]
    struct SimpleMessage {
        name: String,
        age: Option<u16>,
    }

    #[test]
    fn test_valid_message_all_fields() {
        let data = SimpleMessage {
            name: "Charlie".to_string(),
            age: Some(28),
        };

        let raw = to_bytes(&data).expect("Unable to serialize");
        // Taken from one of the bebop reference implementations
        let expected: Vec<u8> = vec![
            16, 0, 0, 0, 1, 7, 0, 0, 0, 67, 104, 97, 114, 108, 105, 101, 2, 28, 0, 0,
        ];
        assert_eq!(raw, expected);
    }

    #[test]
    fn test_valid_message_some_fields() {
        let data = SimpleMessage {
            name: "Charlie".to_string(),
            age: None,
        };

        let raw = to_bytes(&data).expect("Unable to serialize");
        // Taken from one of the bebop reference implementations
        let expected: Vec<u8> = vec![
            13, 0, 0, 0, 1, 7, 0, 0, 0, 67, 104, 97, 114, 108, 105, 101, 0,
        ];
        assert_eq!(raw, expected);
    }

    #[derive(Serialize)]
    #[allow(dead_code)]
    enum Fun {
        Not,
        Somewhat,
        Really,
    }

    #[derive(Serialize)]
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
        // We are only inserting one entry into each map so the byte array will
        // match (ordering doesn't matter for hash maps and so they could get
        // encoded in different ways)
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
        let data = Complex {
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

        let raw = to_bytes(&data).expect("Unable to serialize");
        // Taken from one of the bebop reference implementations
        let expected: Vec<u8> = vec![
            124, 0, 0, 0, 1, 7, 0, 0, 0, 67, 104, 97, 114, 108, 105, 101, 2, 1, 0, 0, 0, 3, 1, 0,
            0, 0, 3, 0, 0, 0, 111, 110, 101, 3, 0, 0, 0, 79, 110, 101, 16, 0, 4, 1, 0, 0, 0, 3, 0,
            0, 0, 111, 110, 101, 9, 0, 0, 0, 1, 3, 0, 0, 0, 79, 110, 101, 0, 5, 2, 0, 0, 0, 218,
            15, 73, 64, 77, 248, 45, 64, 6, 1, 7, 253, 255, 8, 42, 0, 0, 0, 9, 21, 205, 91, 7, 0,
            0, 0, 0, 10, 3, 0, 11, 42, 0, 0, 0, 12, 21, 205, 91, 7, 0, 0, 0, 0, 13, 17, 14, 74,
            216, 18, 77, 251, 33, 9, 64, 0,
        ];
        assert_eq!(raw, expected);
    }
}
