#[allow(unused_imports)]
#[macro_use] extern crate pretty_assertions;

pub mod tokens;
pub mod parser;
pub mod string_table;
pub use tokens::get_tokens;
pub use parser::parse_program;

// cargo run --example read-file examples/fibonacci.kal
