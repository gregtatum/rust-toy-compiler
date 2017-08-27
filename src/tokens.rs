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

pub enum CharacterType {
    Whitespace,
    Alpha,
    Numeric,
    Unknown,
    Pound,
    Period,
}

fn char_to_enum(character: &char) -> CharacterType {
    if character.is_numeric() {
        return CharacterType::Numeric;
    }
    if character.is_alphabetic() {
        return CharacterType::Alpha;
    }
    if character.is_whitespace() {
        return CharacterType::Whitespace
    }
    if *character == '#' {
        return CharacterType::Pound
    }
    if *character == '.' {
        return CharacterType::Period
    }
    return CharacterType::Unknown
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Token::Eof => write!(f, "(eof)"),
            &Token::Def => write!(f, "(def)"),
            &Token::Extern => write!(f, "(extern)"),
            &Token::Identifier(_) => write!(f, "(identifier)"),
            &Token::Number(n) => write!(f, "(number {})", n),
            &Token::Char(c) => write!(f, "(char {})", c),
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
            CharacterType::Whitespace => continue,
            CharacterType::Pound => skip_to_end_of_line(&mut characters),
            CharacterType::Numeric => tokens.push(
                Token::Number(get_number(&mut characters, character))
            ),
            CharacterType::Alpha => {
                let word = get_word(&mut characters, &character);
                if word == "def" {
                    tokens.push(Token::Def);
                    continue;
                }
                if word == "extern" {
                    tokens.push(Token::Extern);
                    continue;
                }
                tokens.push(Token::Identifier(string_table.take_string(word)));
            },
            _ => tokens.push(Token::Char(character))
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
        CharacterType::Alpha | CharacterType::Numeric => {
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
        CharacterType::Numeric => {
            word.push(character);
            characters.next();
        },
        CharacterType::Period => {
            word.push(character);
            characters.next();

            // Find the remaining digits.
            iter_peek_match!(characters, character => {
                CharacterType::Numeric => {
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
