#![allow(dead_code)]
#![allow(unused_variables)]

use std::fmt;
use std::str::Chars;
use std::iter::Peekable;
use string_table::StringTable;
use string_table::StringIndex;

#[derive(Debug, Clone)]
pub enum Token {
    Eof,
    Def,
    Extern,
    Identifier(StringIndex),
    Number(f64),
    Char(Character),
}

#[derive(Debug, Clone, Copy)]
pub enum Character {
    Whitespace,
    Alpha,
    Numeric,
    Unknown,
    Pound,
    Period,
    LeftParens,
    RightParens,
    Comma,
    LessThan,
    GreaterThan,
    Plus,
    Minus,
}

fn char_to_enum(character: &char) -> Character {
    if character.is_numeric() {
        return Character::Numeric;
    }
    if character.is_alphabetic() {
        return Character::Alpha;
    }
    if character.is_whitespace() {
        return Character::Whitespace;
    }
    if *character == '#' {
        return Character::Pound;
    }
    if *character == '.' {
        return Character::Period;
    }
    if *character == '(' {
        return Character::LeftParens;
    }
    if *character == ')' {
        return Character::RightParens;
    }
    if *character == '<' {
        return Character::LessThan;
    }
    if *character == '>' {
        return Character::GreaterThan;
    }
    if *character == '+' {
        return Character::Plus;
    }
    if *character == '-' {
        return Character::Minus;
    }
    return Character::Unknown
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Token::Eof => write!(f, "(eof)"),
            &Token::Def => write!(f, "(def)"),
            &Token::Extern => write!(f, "(extern)"),
            &Token::Identifier(_) => write!(f, "(identifier)"),
            &Token::Number(n) => write!(f, "(number {})", n),
            &Token::Char(c) => write!(f, "(char {:?})", c),
        }
    }
}

macro_rules! iter_next {
    ($iter:ident, $item:ident => $body:expr) => {
        loop {
            match $iter.next() {
                Some($item) => $body,
                None => break,
            }
        }
    };
}

macro_rules! iter_peek_match {
    ($iterator:ident, $item:ident => $match_body:tt) => {
        loop {
            match $iterator.peek() {
                Some(&$item) => {
                    match char_to_enum(&$item) $match_body;
                },
                None => break,
            }
        };
    }
}

pub fn get_tokens(text: &str) -> Vec<Token> {
    let mut string_table = StringTable::new();
    let mut tokens = Vec::new();

    // Provide a shareable, mutable iterator over the characters.
    let mut characters = IntoIterator::into_iter(text.chars()).peekable();

    // This macro is convenience sugar to run the iterator without resorting
    // to a for-in, but keeping it terse.
    iter_next!(characters, character => {
        match char_to_enum(&character) {
            Character::Whitespace => continue,
            Character::Pound => skip_to_end_of_line(&mut characters),
            Character::Numeric => tokens.push(
                Token::Number(get_number(&mut characters, character))
            ),
            Character::Alpha => tokens.push({
                let word = get_word(&mut characters, &character);
                if word == "def" {
                    Token::Def
                } else if word == "extern" {
                    Token::Extern
                } else {
                    Token::Identifier(string_table.take_string(word))
                }
            }),
            Character::Unknown => println!("Unknown character {}", character),
            character_type => tokens.push(Token::Char(character_type))
        }
    });

    tokens
}

fn skip_to_end_of_line(characters: &mut Peekable<Chars>) {
    iter_next!(characters, character => {
        if character == '\n' {
            break;
        }
    })
}

fn get_word(characters: &mut Peekable<Chars>, starting_char: &char) -> String {
    let mut word = String::new();
    word.push(*starting_char);

    iter_peek_match!(characters, character => {
        Character::Alpha | Character::Numeric => {
            word.push(character);
            characters.next();
        },
        _ => break,
    });

    return word;
}

fn get_number(characters: &mut Peekable<Chars>, starting_digit: char) -> f64 {
    let mut word = String::new();
    word.push(starting_digit);

    iter_peek_match!(characters, character => {
        Character::Numeric => {
            word.push(character);
            characters.next();
        },
        Character::Period => {
            word.push(character);
            characters.next();

            // Find the remaining digits.
            iter_peek_match!(characters, character => {
                Character::Numeric => {
                    word.push(character);
                    characters.next();
                },
                _ => break,
            });
            break;
        },
        _ => break,
    });

    return word.parse::<f64>().expect("Number should be validly parsed");
}
