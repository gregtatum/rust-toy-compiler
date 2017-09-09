extern crate term_painter;
extern crate kal;

use term_painter::ToStyle;
use term_painter::Color::*;

use std::env;
use std::fs::File;
use std::io::prelude::*;

fn get_text() -> String {
    let args: Vec<_> = env::args().collect();
    match args.last() {
        Some(path) => {
            let mut file = File::open(path).expect("File not found");
            let mut text = String::new();
            file.read_to_string(&mut text).expect("Could not read the file.");
            text
        },
        None => {
            panic!("Expected the path to the file as the first argument.");
        },
    }
}

fn main() {
    let text = get_text();
    println!(
        "\n{}\n\n{}\n",
        Green.paint("----------------------------------------------------\nFound the text:"),
        White.bold().paint(&text),
    );

    let tokens = kal::get_tokens(&text);
    println!("{:?}", Blue.bold().paint(&tokens));

    let ast = kal::parse_program(tokens);
    println!("\n\n{:#?}", Magenta.bold().paint(ast));
}
