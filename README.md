# Serde Bebop

A [Serde](https://serde.rs) implementation of the [Bebop
protocol](https://github.com/RainwayApp/bebop). It is purely experimental and I am not yet sure if I
will continue maintaining it

## What this isn't

To be clear, this is only a serialization and deserialization implementation for Rust, it doesn't
have anything else for compiling `.bop` files or generating code/structs other than the
deserialization code

## How to use

You'll need the following two imports in your `Cargo.toml`

```toml
[dependencies]
serde_bebop = "0.1"
serde = { version = "1.0", features = ["derive"] }
```

Then, in your Rust code:

```rust
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
struct Person {
    name: String,
    age: u16,
}

fn main() {
    let data = Person {
        name: "Charlie".to_string(),
        age: 28,
    };

    let raw = serde_bebop::to_bytes(&data).expect("Unable to serialize");
    println!("{:?}", raw);

    let deserialized: Person = serde_bebop::from_bytes(&raw).expect("Unable to deserialize");
    println!("{:?}", deserialized);
}
```

## Missing features

There is currently no support for the GUID and Date types of Bebop as I haven't looked in to how to
do that in Serde yet.
