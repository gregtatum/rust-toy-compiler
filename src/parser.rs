use string_table::StringIndex;
use tokens::Token;
use tokens::Character;
use std::iter::Peekable;
use std::vec::IntoIter;

pub struct CallExpressionData {
    pub callee: StringIndex,
    pub args: Vec<SyntaxNode>
}

pub struct PrototypeData {
    pub name: StringIndex,
    pub args: Vec<StringIndex>
}

pub struct BinaryExpressionData {
    pub operation: StringIndex,
    pub lhs: Box<SyntaxNode>,
    pub rhs: Box<SyntaxNode>,
}

pub struct FunctionData {
    prototype: Box<SyntaxNode>,
    body: Box<SyntaxNode>,
}

pub enum SyntaxNode {
    Number(f64),
    Variable(StringIndex),
    BinaryExpression(BinaryExpressionData),
    CallExpression(CallExpressionData),
    Prototype(PrototypeData),
    Function(FunctionData),
}

pub enum ParseError {
    UnmatchedParen,
    UnknownArgumentValue,
    NotExpressionToken,
}

type PeekableTokens = Peekable<IntoIter<Token>>;

fn parse_number(mut tokens: &mut PeekableTokens, number: f64) -> Result<SyntaxNode, ParseError> {
    tokens.next();
    Ok(SyntaxNode::Number(number))
}

fn parse_parens(mut tokens: &mut PeekableTokens) -> Result<SyntaxNode, ParseError> {
    // Skip `(`
    tokens.next();
    match parse_expression(&mut tokens) {
        Ok(expression) => {
            // Skip `)`
            match tokens.next() {
                Some(Token::Char(Character::RightParens)) => Ok(expression),
                _ => Err(ParseError::UnmatchedParen),
            }
        },
        Err(error) => Err(error),
    }
}

fn parse_identifier(
    mut tokens: &mut PeekableTokens,
    identifier_string: StringIndex
) -> Result<SyntaxNode, ParseError> {
    // Skip the identifier.
    tokens.next();

    match tokens.peek() {
        // Match for function call
        Some(&Token::Char(Character::LeftParens)) => {
            tokens.next();
            let mut args = Vec::new();
            loop {
                match parse_expression(&mut tokens) {
                    Ok(expression) => args.push(expression),
                    Err(error) => return Err(error)
                };
                match tokens.next() {
                    Some(Token::Char(Character::RightParens)) => {
                        return Ok(SyntaxNode::CallExpression(CallExpressionData {
                            callee: identifier_string,
                            args: args,
                        }))
                    }
                    Some(Token::Char(Character::Comma)) => continue,
                    _ => return Err(ParseError::UnknownArgumentValue),
                };
            }
        },
        _ => return Ok(SyntaxNode::Variable(identifier_string))
    }
}

fn parse_expression(mut tokens: &mut PeekableTokens) -> Result<SyntaxNode, ParseError> {
    // TODO
    Ok(SyntaxNode::Number(0.0))
}

fn parse_primary(mut tokens: &mut PeekableTokens) -> Result<SyntaxNode, ParseError> {
    match tokens.peek() {
        Some(&Token::Identifier(string_index)) => parse_identifier(&mut tokens, string_index),
        Some(&Token::Number(number)) => parse_number(&mut tokens, number),
        Some(&Token::Char(Character::LeftParens)) => parse_parens(&mut tokens),
        _ => Err(ParseError::NotExpressionToken),
    }
}

fn parse_tokens(tokens_vec: Vec<Token>) -> Result<SyntaxNode, ParseError> {
    // Provide a shareable, mutable iterator over the characters.
    let mut tokens = IntoIterator::into_iter(tokens_vec).peekable();
    parse_primary(&mut tokens)
}
