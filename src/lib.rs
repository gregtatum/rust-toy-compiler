#[macro_use] extern crate pretty_assertions;

pub mod tokens;
pub mod parser;
pub mod string_table;
pub use tokens::get_tokens;

// cargo run --example read-file examples/fibonacci.kal
