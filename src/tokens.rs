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
    Char(char),
}

#[derive(Debug, Clone, Copy)]
pub enum Character {
    Whitespace,
    Alpha,
    Numeric,
    Value(char),
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
    return Character::Value(character.clone());
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Token::Eof => write!(f, "(eof)"),
            &Token::Def => write!(f, "(def)"),
            &Token::Extern => write!(f, "(extern)"),
            &Token::Identifier(_) => write!(f, "(identifier)"),
            &Token::Number(n) => write!(f, "(number {})", n),
            &Token::Char(ref value) => write!(f, "(char {:?})", value),
        }
    }
}

// This is convenience sugar over peeking and matching an enum.
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

    loop {
        match characters.next() {
            Some(character) => {
                match char_to_enum(&character) {
                    Character::Whitespace => continue,
                    Character::Value('#') => skip_to_end_of_line(&mut characters),
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
                    Character::Value(value) => tokens.push(Token::Char(value))
                }
            },
            None => break,
        }
    }

    tokens
}

fn skip_to_end_of_line(characters: &mut Peekable<Chars>) {
    loop {
        match characters.next() {
            Some(character) => {
                if character == '\n' {
                    break;
                }
            },
            None => break,
        }
    }
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
        Character::Value('.') => {
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
