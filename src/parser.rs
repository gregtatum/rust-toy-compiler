use string_table::StringIndex;
use string_table::StringTable;
use tokens::Token;
use tokens::Character;
use std::iter::Peekable;
use std::vec::IntoIter;

#[derive(Debug,PartialEq)]
pub struct CallExpressionData {
    pub callee: StringIndex,
    pub args: Vec<SyntaxNode>
}

#[derive(Debug,PartialEq)]
pub struct PrototypeData {
    pub name: StringIndex,
    pub args: Vec<StringIndex>
}

#[derive(Debug,PartialEq)]
pub struct BinaryExpressionData {
    pub operation: char,
    pub lhs: Box<SyntaxNode>,
    pub rhs: Box<SyntaxNode>,
}

#[derive(Debug,PartialEq)]
pub struct FunctionData {
    prototype: Box<SyntaxNode>,
    body: Box<SyntaxNode>,
}

#[derive(Debug,PartialEq)]
pub enum SyntaxNode {
    Number(f64),
    Variable(StringIndex),
    BinaryExpression(BinaryExpressionData),
    CallExpression(CallExpressionData),
    Prototype(PrototypeData),
    Function(FunctionData),
}

#[derive(Debug,PartialEq)]
pub enum ParseError {
    UnmatchedParen,
    UnknownArgumentValue,
    NotExpressionToken,
    NoArgIdentifier,
    ExpectedOpeningParens,
    NoFunctionName,
}

type PeekableTokens = Peekable<IntoIter<Token>>;

fn parse_number(mut tokens: &mut PeekableTokens, number: f64) -> Result<SyntaxNode, ParseError> {
    tokens.next();
    Ok(SyntaxNode::Number(number))
}

fn parse_parens(mut tokens: &mut PeekableTokens) -> Result<SyntaxNode, ParseError> {
    // Skip `(`
    tokens.next();
    let expression = parse_expression(&mut tokens)?;

    // Skip `)`
    match tokens.next() {
        Some(Token::Char(')')) => Ok(expression),
        _ => Err(ParseError::UnmatchedParen),
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
        Some(&Token::Char('(')) => {
            tokens.next();
            let mut args = Vec::new();
            loop {
                match parse_expression(&mut tokens) {
                    Ok(expression) => args.push(expression),
                    Err(error) => return Err(error)
                };
                match tokens.next() {
                    Some(Token::Char(')')) => {
                        return Ok(SyntaxNode::CallExpression(CallExpressionData {
                            callee: identifier_string,
                            args: args,
                        }))
                    }
                    Some(Token::Char(',')) => continue,
                    _ => return Err(ParseError::UnknownArgumentValue),
                };
            }
        },
        _ => return Ok(SyntaxNode::Variable(identifier_string))
    }
}

fn parse_expression(mut tokens: &mut PeekableTokens) -> Result<SyntaxNode, ParseError> {
    let lhs = parse_primary(&mut tokens)?;
    parse_binary_operation_rhs(&mut tokens, 0, lhs)
}

fn parse_binary_operation_rhs(
    mut tokens: &mut PeekableTokens,
    lhs_precedence: i32,
    mut lhs: SyntaxNode
) -> Result<SyntaxNode, ParseError> {
    loop {
        let operator_precedence = get_token_precedence(&tokens.peek());
        if operator_precedence < lhs_precedence {
            return Ok(lhs);
        }
        // This is a binary operation, skip over the token.
        let operation_token = tokens.next().unwrap();

        // Parse the rhs expression
        let next_expression = parse_primary(&mut tokens)?;
        if operator_precedence < get_token_precedence(&tokens.peek()) {
            return parse_binary_operation_rhs(&mut tokens, operator_precedence + 1, next_expression)
        } else {
            return Ok(next_expression)
        }
    }
}

fn parse_primary(mut tokens: &mut PeekableTokens) -> Result<SyntaxNode, ParseError> {
    match tokens.peek() {
        Some(&Token::Identifier(string_index)) => parse_identifier(&mut tokens, string_index),
        Some(&Token::Number(number)) => parse_number(&mut tokens, number),
        Some(&Token::Char('(')) => parse_parens(&mut tokens),
        _ => Err(ParseError::NotExpressionToken),
    }
}

fn get_token_precedence(token: &Option<&Token>) -> i32 {
    match token {
        &Some(&Token::Char('<')) => 10,
        &Some(&Token::Char('+')) => 20,
        &Some(&Token::Char('-')) => 30,
        &Some(&Token::Char('*')) => 40,
        _ => -1,
    }
}

fn get_operator_precedence(character: &char) -> i32 {
    match character {
        &'<' => 10,
        &'+' => 20,
        &'-' => 30,
        &'*' => 40,
        &_ => panic!("There should not be an unknown char"),
    }
}

fn parse_tokens(tokens_vec: Vec<Token>) -> Result<SyntaxNode, ParseError> {
    // Provide a shareable, mutable iterator over the characters.
    let mut tokens = IntoIterator::into_iter(tokens_vec).peekable();
    parse_primary(&mut tokens)
}

fn parse_prototype(mut tokens: &mut PeekableTokens) -> Result<SyntaxNode, ParseError> {
    match tokens.next() {
        Some(Token::Identifier(function_name)) => {
            match tokens.next() {
                Some(Token::Char('(')) => {
                    let mut args = Vec::new();
                    loop {
                        match tokens.next() {
                            Some(Token::Char(')')) => break,
                            Some(Token::Identifier(arg_name)) => args.push(arg_name),
                            _ => return Err(ParseError::NoArgIdentifier),
                        }
                        match tokens.peek() {
                            Some(&Token::Char(',')) => { tokens.next(); },
                            _ => {},
                        }
                    }
                    Ok(SyntaxNode::Prototype(PrototypeData {
                        name: function_name,
                        args: args,
                    }))
                },
                _ => Err(ParseError::ExpectedOpeningParens),
            }
        },
        _ => Err(ParseError::NoFunctionName),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    mod parse_prototype {
        use super::*;

        #[test]
        fn no_args() {
            assert_eq!(
                parse_prototype(
                    &mut IntoIterator::into_iter(vec![
                        Token::Identifier(1),
                        Token::Char('('),
                        Token::Char(')'),
                    ]).peekable()
                ),
                Ok(SyntaxNode::Prototype(PrototypeData {
                    name: 1,
                    args: Vec::new()
                }))
            );
        }

        #[test]
        fn one_arg() {
            assert_eq!(
                parse_prototype(
                    &mut IntoIterator::into_iter(vec![
                        Token::Identifier(1),
                        Token::Char('('),
                        Token::Identifier(4),
                        Token::Char(')'),
                    ]).peekable()
                ),
                Ok(SyntaxNode::Prototype(PrototypeData {
                    name: 1,
                    args: vec![4]
                }))
            );
        }

        #[test]
        fn multiple_args() {
            assert_eq!(
                parse_prototype(
                    &mut IntoIterator::into_iter(vec![
                        Token::Identifier(1),
                        Token::Char('('),
                        Token::Identifier(4),
                        Token::Char(','),
                        Token::Identifier(5),
                        Token::Char(','),
                        Token::Identifier(6),
                        Token::Char(')'),
                    ]).peekable()
                ),
                Ok(SyntaxNode::Prototype(PrototypeData {
                    name: 1,
                    args: vec![4, 5, 6]
                }))
            );
        }

        #[test]
        fn no_arg_identifier() {
            assert_eq!(
                parse_prototype(
                    &mut IntoIterator::into_iter(vec![
                        Token::Identifier(1),
                        Token::Char('('),
                        Token::Char(','),
                        Token::Char(','),
                        Token::Identifier(5),
                        Token::Char(')'),
                    ]).peekable()
                ),
                Err(ParseError::NoArgIdentifier)
            );
        }

        #[test]
        fn expected_opening_parens() {
            assert_eq!(
                parse_prototype(
                    &mut IntoIterator::into_iter(vec![
                        Token::Identifier(1),
                        Token::Identifier(5),
                    ]).peekable()
                ),
                Err(ParseError::ExpectedOpeningParens)
            );
        }

        #[test]
        fn no_function_name() {
            assert_eq!(
                parse_prototype(
                    &mut IntoIterator::into_iter(vec![
                        Token::Char('(')
                    ]).peekable()
                ),
                Err(ParseError::NoFunctionName)
            );
        }
    }
}
