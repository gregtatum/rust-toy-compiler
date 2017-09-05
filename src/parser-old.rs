#![allow(dead_code,unused_variables,unused_imports)]

use string_table::StringIndex;
use string_table::StringTable;
use tokens::Token;
use tokens::Character;
use std::iter::Peekable;
use std::vec::IntoIter;

#[derive(Debug,PartialEq,Clone)]
pub struct CallExpressionData {
    pub callee: StringIndex,
    pub args: Vec<SyntaxNode>
}

#[derive(Debug,PartialEq,Clone)]
pub struct PrototypeData {
    pub name: Option<StringIndex>,
    pub args: Vec<StringIndex>
}

#[derive(Debug,PartialEq,Clone)]
pub struct BinaryExpressionData {
    pub operation: char,
    pub lhs: Box<SyntaxNode>,
    pub rhs: Box<SyntaxNode>,
}

#[derive(Debug,PartialEq,Clone)]
pub struct DefinitionData {
    prototype: Box<SyntaxNode>,
    body: Box<SyntaxNode>,
}

#[derive(Debug,PartialEq,Clone)]
pub enum SyntaxNode {
    Number(f64),
    Variable(StringIndex),
    BinaryExpression(BinaryExpressionData),
    CallExpression(CallExpressionData),
    Prototype(PrototypeData),
    Definition(DefinitionData),
    Empty,
}

#[derive(Debug,PartialEq,Clone)]
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

fn parse_primary(mut tokens: &mut PeekableTokens) -> Result<SyntaxNode, ParseError> {
    println!("parse_primary token:       {:?}", tokens.peek());
    match tokens.peek() {
        Some(&Token::Identifier(string_index)) => parse_identifier(&mut tokens, string_index),
        Some(&Token::Number(number)) => parse_number(&mut tokens, number),
        Some(&Token::Char('(')) => parse_parens(&mut tokens),
        _ => Err(ParseError::NotExpressionToken),
    }
}

fn parse_expression(mut tokens: &mut PeekableTokens) -> Result<SyntaxNode, ParseError> {
    println!("parse_expression           {:?}", tokens.peek());
    let lhs = parse_primary(&mut tokens)?;
    println!("parse_expression lhs:      {:?}", lhs);
    parse_binary_operation_rhs(&mut tokens, 0, lhs)
}

fn parse_binary_operation_rhs(
    mut tokens: &mut PeekableTokens,
    lhs_precedence: i32,
    lhs: SyntaxNode
) -> Result<SyntaxNode, ParseError> {
    println!("parse_binary_operation_rhs {:?} {:?}", tokens.peek(), lhs.clone());

    let operator_precedence = get_token_precedence(&tokens.peek());
    println!("operator_precedence        {:?} < lhs_precedence {:?}", operator_precedence, lhs_precedence);
    if operator_precedence < lhs_precedence {
        return Ok(lhs);
    }

    // This is a binary operation, skip over the token.
    let operation_char = match tokens.next() {
        Some(Token::Char(op_char)) => op_char,
        _ => panic!("This should be an operation character.")
    };

    // Parse the rhs expression
    let mut rhs = parse_primary(&mut tokens)?;
    println!("rhs                        {:?}", rhs);

    // If the binary operation is stronger, swap out the result recursively.
    if operator_precedence < get_token_precedence(&tokens.peek()) {
        println!("Recurse into parse_binary_operation_rhs");
        rhs = parse_binary_operation_rhs(&mut tokens, operator_precedence + 1, rhs)?;
    }

    println!("{:?}", SyntaxNode::BinaryExpression(BinaryExpressionData {
        operation: operation_char,
        lhs: Box::new(lhs.clone()),
        rhs: Box::new(rhs.clone()),
    }));

    Ok(SyntaxNode::BinaryExpression(BinaryExpressionData {
        operation: operation_char,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }))
}

fn get_token_precedence(token: &Option<&Token>) -> i32 {
    match token {
        &Some(&Token::Char('<')) => 10,
        &Some(&Token::Char('+')) => 20,
        &Some(&Token::Char('-')) => 20,
        &Some(&Token::Char('*')) => 40,
        _ => -1,
    }
}

fn token_is_binary_operation(token: &Option<&Token>) -> i32 {
    match token {
        &Some(
            &Token::Char('+') |
            &Token::Char('-') |
            &Token::Char('*') |
        ) => true,
        _ => false,
    }
}

fn parse_tokens (tokens_vec: Vec<Token>) -> Result<SyntaxNode, ParseError> {
    // Provide a shareable, mutable iterator over the characters.
    let mut tokens = IntoIterator::into_iter(tokens_vec).peekable();
    let mut ast = SyntaxNode::Empty;
    loop {
        println!("parse_tokens               {:?}", tokens.peek());
        match tokens.peek() {
            None => break,
            Some(&Token::Char(';')) => {
                tokens.next();
            },
            Some(&Token::Def) => {
                ast = parse_definition(&mut tokens)?;
            },
            Some(&Token::Extern) => {
                ast = parse_extern(&mut tokens)?;
            },
            _ => {
                ast = parse_top_level_expression(&mut tokens)?;
            },
        }
    }
    return Ok(ast);
}

fn parse_top_level_expression(mut tokens: &mut PeekableTokens) -> Result<SyntaxNode, ParseError> {
    println!("parse_top_level_expression {:?}", tokens.peek());
    let expression = parse_expression(&mut tokens)?;
    // Place the expression in an anonymous definition.
    Ok(SyntaxNode::Definition(DefinitionData {
        prototype: Box::new(SyntaxNode::Prototype(PrototypeData {
            name: None,
            args: Vec::new()
        })),
        body: Box::new(expression),
    }))
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
                        name: Some(function_name),
                        args: args,
                    }))
                },
                _ => Err(ParseError::ExpectedOpeningParens),
            }
        },
        _ => Err(ParseError::NoFunctionName),
    }
}

fn parse_definition(mut tokens: &mut PeekableTokens) -> Result<SyntaxNode, ParseError> {
    tokens.next(); // skip "def"
    Ok(SyntaxNode::Definition(DefinitionData {
        prototype: Box::new(parse_prototype(&mut tokens)?),
        body: Box::new(parse_expression(&mut tokens)?),
    }))
}

fn parse_extern(mut tokens: &mut PeekableTokens) -> Result<SyntaxNode, ParseError> {
    tokens.next();
    return parse_prototype(&mut tokens);
}

#[cfg(test)]
mod test {
    use super::*;

    mod test_helpers {
        use super::*;

        pub fn binop_result_to_text(result: &Result<SyntaxNode, ParseError>) -> String {
            match result {
                &Ok(ref ast) => binop_to_text(&ast),
                &Err(ref err) => format!("{:?}", err)
            }
        }

        pub fn binop_to_text(ast: &SyntaxNode) -> String {
            match ast {
                &SyntaxNode::BinaryExpression(BinaryExpressionData {
                    operation: ref op,
                    lhs: ref lhs,
                    rhs: ref rhs
                }) => {
                    let lhs_text = binop_to_text(lhs);
                    let rhs_text = binop_to_text(rhs);
                    format!("({} {} {})", op, lhs_text, rhs_text)
                }
                &SyntaxNode::Variable(v) => format!("{}", v),
                _ => format!("{:?}", ast),
            }
        }

        pub fn anonymous_fn_to_binop_text(result: &Result<SyntaxNode, ParseError>) -> String {
            match result {
                &Ok(SyntaxNode::Definition(DefinitionData {
                    prototype: ref prototype,
                    body: ref body,
                })) => {
                    match **prototype {
                        SyntaxNode::Prototype(PrototypeData {
                            name: None,
                            ..
                        }) => {
                            binop_to_text(body)
                        },
                        _ => format!("Not an anonymous function {:?}", prototype)
                    }

                },
                &Ok(ref ast) => format!("Not an anonymous function {:?}", ast),
                &Err(ref err) => format!("{:?}", err),
            }
        }
    }

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
                    name: Some(1),
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
                    name: Some(1),
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
                    name: Some(1),
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

    mod parse_definition {
        use super::*;

        // #[test]
        fn no_args() {
            assert_eq!(
                parse_definition(
                    &mut IntoIterator::into_iter(vec![
                        Token::Def,
                        Token::Identifier(1),
                        Token::Char('('),
                        Token::Char(')'),
                    ]).peekable()
                ),
                Ok(SyntaxNode::Definition(DefinitionData {
                    prototype: Box::new(SyntaxNode::Prototype(PrototypeData {
                        name: Some(1),
                        args: Vec::new()
                    })),
                    body: Box::new(SyntaxNode::Empty),
                }))
            );
        }
    }

    mod parse_expression {
        use super::*;

        #[test]
        fn single_variable() {
            assert_eq!(
                test_helpers::binop_result_to_text(&parse_expression(
                    &mut IntoIterator::into_iter(vec![
                        Token::Identifier(1),
                    ]).peekable()
                )),
                "1"
            );
        }

        #[test]
        fn normal_binary_op() {
            assert_eq!(
                test_helpers::binop_result_to_text(&parse_expression(
                    &mut IntoIterator::into_iter(vec![
                        Token::Identifier(1),
                        Token::Char('+'),
                        Token::Identifier(2),
                    ]).peekable()
                )),
                "(+ 1 2)"
            );
        }

        #[test]
        fn two_binary_ops() {
            assert_eq!(
                test_helpers::binop_result_to_text(&parse_expression(
                    &mut IntoIterator::into_iter(vec![
                        Token::Identifier(1),
                        Token::Char('+'),
                        Token::Identifier(2),
                        Token::Char('+'),
                        Token::Identifier(3),
                    ]).peekable()
                )),
                "(+ 1 2)"
            );
        }

        #[test]
        fn two_ops_of_different_precedence() {
            assert_eq!(
                test_helpers::binop_result_to_text(&parse_expression(
                    &mut IntoIterator::into_iter(vec![
                        Token::Identifier(1),
                        Token::Char('+'),
                        Token::Identifier(2),
                        Token::Char('*'),
                        Token::Identifier(3),
                    ]).peekable()
                )),
                "(+ 1 (* 2 3))"
            );
        }
    }

    mod parse_tokens {
        use super::*;

        #[test]
        fn single_variable() {
            assert_eq!(
                test_helpers::anonymous_fn_to_binop_text(&parse_tokens(
                    vec![
                        Token::Identifier(1),
                    ]
                )),
                "1"
            );
        }

        #[test]
        fn normal_binary_op() {
            assert_eq!(
                test_helpers::anonymous_fn_to_binop_text(&parse_tokens(
                    vec![
                        Token::Identifier(1),
                        Token::Char('+'),
                        Token::Identifier(2),
                    ]
                )),
                "(+ 1 2)"
            );
        }

        #[test]
        fn two_binary_ops() {
            println!("Parsing: 1 + 2 - 3");
            println!("Result: {}", test_helpers::anonymous_fn_to_binop_text(&parse_tokens(
                vec![
                    Token::Identifier(1),
                    Token::Char('+'),
                    Token::Identifier(2),
                    Token::Char('-'),
                    Token::Identifier(3),
                ]
            )));
            assert_eq!(
                test_helpers::anonymous_fn_to_binop_text(&parse_tokens(
                    vec![
                        Token::Identifier(1),
                        Token::Char('+'),
                        Token::Identifier(2),
                        Token::Char('-'),
                        Token::Identifier(3),
                    ]
                )),
                "(- (+ 1 2) 3)"
            );
        }

        #[test]
        fn two_ops_of_different_precedence() {
            assert_eq!(
                test_helpers::anonymous_fn_to_binop_text(&parse_tokens(
                    vec![
                        Token::Identifier(1),
                        Token::Char('+'),
                        Token::Identifier(2),
                        Token::Char('*'),
                        Token::Identifier(3),
                    ]
                )),
                "(+ 1 (* 2 3))"
            );
        }
    }

}
