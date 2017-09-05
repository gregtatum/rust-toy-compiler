#![allow(dead_code,unused_variables,unused_imports)]

use std::fmt;
use string_table::StringIndex;
use string_table::StringTable;
use tokens::Token;
use tokens::Character;
use std::iter::Peekable;
use std::vec::IntoIter;

#[derive(Debug,PartialEq,Clone)]
pub struct ProgramData {
    pub body: Vec<SyntaxNode>
}

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
    Program(ProgramData),
    Number(f64),
    Variable(StringIndex),
    BinaryExpression(BinaryExpressionData),
    CallExpression(CallExpressionData),
    Prototype(PrototypeData),
    Definition(DefinitionData),
    TODO,
}

// impl fmt::Display for SyntaxNode {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//
//         }
//         write!(f, "({}, {})", self.x, self.y)
//     }
// }


#[derive(Debug,PartialEq,Clone)]
pub enum ParseError {
    UnmatchedParen,
    UnknownArgumentValue,
    NotExpressionToken,
    NoArgIdentifier,
    ExpectedOpeningParens,
    NoFunctionName,
    UnknownCharacterInProgram,
    ExpectedTerm,
}

type PeekableTokens = Peekable<IntoIter<Token>>;
type ResultNode = Result<SyntaxNode, ParseError>;

// FUNCTION -> "function" (identifier, ...) {
//               BLOCK
//               RETURN_EXPRESSION
//             }
//             e.g. function () { return 0; }
//             e.g. funtion (a, b) { a = a + b; return a }
fn parse_function (tokens: &mut PeekableTokens) -> ResultNode {
    tokens.next();
    Ok(SyntaxNode::TODO)
}

// TERM -> VARIABLE
//       | NUMBER
fn parse_term (tokens: &mut PeekableTokens) -> ResultNode {
    match tokens.next() {
        Some(Token::Identifier(string_index)) => Ok(SyntaxNode::Variable(string_index)),
        Some(Token::Number(number)) => Ok(SyntaxNode::Number(number)),
        _ => Err(ParseError::ExpectedTerm),
    }
}
fn make_binary_expression(operation: char, lhs: SyntaxNode, rhs: SyntaxNode) -> ResultNode {
    Ok(SyntaxNode::BinaryExpression(BinaryExpressionData {
        operation: operation,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }))
}

// EXPRESSION -> VARIABLE
//             | NUMBER
//             | EXPRESSION BINOP EXPRESSION
fn parse_expression (mut tokens: &mut PeekableTokens) -> ResultNode {
    // Given the cases (a + b), (a + b + c), (a * b + c)
    let a = parse_term(tokens)?;

    match if_binary_operator_get_precedence(tokens.peek()) {
        // The next token is not a binary operation.
        None => Ok(a),
        Some(a_b_precedence) => {
            // There is a precedence, we have a binary operation, continue processing.
            match tokens.next() {
                Some(Token::Char(a_b_operation)) => {
                    let b = parse_term(&mut tokens)?;
                    match if_binary_operator_get_precedence(tokens.peek()) {
                        // Handle case (a + b), where no binary expressions follow.
                        None => make_binary_expression(a_b_operation, a, b),
                        // Handle (a + b + c) where operations keep on following.
                        Some(b_c_precedence) => {
                            match tokens.next() {
                                Some(Token::Char(b_c_operation)) => {
                                    let c = parse_expression(&mut tokens)?;
                                    if a_b_precedence >= b_c_precedence {
                                        // (a + b + c) -> (+ (+ a b) c)
                                        // (a * b + c) -> (+ (* a b) c)
                                        make_binary_expression(
                                            b_c_operation,
                                            make_binary_expression(a_b_operation, a, b)?,
                                            c
                                        )
                                    } else {
                                        // (a + b * c) (+ a (* b c))
                                        make_binary_expression(
                                            a_b_operation,
                                            a,
                                            make_binary_expression(b_c_operation, b, c)?
                                        )
                                    }
                                },
                                _ => panic!("An operation char should have been extracted"),
                            }
                        }
                    }
                },
                _ => panic!("An operation char should have been extracted"),
            }
        }
    }
}

fn if_binary_operator_get_precedence(token: Option<&Token>) -> Option<i32> {
    match token {
        Some(&Token::Char('+')) => Some(1),
        Some(&Token::Char('-')) => Some(1),
        Some(&Token::Char('*')) => Some(2),
        Some(&Token::Char('/')) => Some(2),
        _ => None,
    }
}

// PROGRAM -> FUNCTION    e.g. function foo() {}
//         |  EXPRESSION  e.g. x + y
fn parse_program (tokens_vec: Vec<Token>) -> ResultNode {
    // Provide a shareable, mutable iterator over the characters.
    let mut tokens = IntoIterator::into_iter(tokens_vec).peekable();

    // Collect all of the top level functions and expressions.
    let mut body: Vec<SyntaxNode> = Vec::new();

    loop {
        match tokens.peek() {
            // Skip any top level semi-colons.
            Some(&Token::Char(';')) => {
                tokens.next();
            },
            // Process functions, which are only allowed at the top level.
            Some(&Token::Function) => {
                body.push(parse_function(&mut tokens)?);
            },
            // This is some kind of identifier or number, parse it as an expression.
            Some(&Token::Identifier(_))
            | Some(&Token::Number(_)) => {
                body.push(parse_expression(&mut tokens)?);
            },
            // This is an unexpected character, error out.
            Some(&Token::Char(_)) => {
                return Err(ParseError::UnknownCharacterInProgram);
            },
            // The end of the token stream was reached, break the loop.
            None => break,
        }
    }

    // Take all of the SyntaxNodes generated by the above the loop, and package into a Program.
    return Ok(SyntaxNode::Program(ProgramData {
        body: body
    }))
}

#[cfg(test)]
mod test {
    use super::*;
    use tokens::get_tokens;

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
                    ref lhs,
                    ref rhs
                }) => {
                    let lhs_text = binop_to_text(lhs);
                    let rhs_text = binop_to_text(rhs);
                    format!("({} {} {})", op, lhs_text, rhs_text)
                }
                &SyntaxNode::Variable(v) => format!("{}", v),
                _ => format!("{:?}", ast),
            }
        }

        pub fn program_to_binop_text(result: &Result<SyntaxNode, ParseError>) -> String {
            match result {
                &Ok(SyntaxNode::Program(ProgramData { ref body })) => {
                    match body.get(0) {
                        Some(ref node) => binop_to_text(&node),
                        _ => format!("Could not get a node out of the program"),
                    }
                },
                &Ok(ref node) => format!("Not a program node {:?}", node),
                &Err(ref err) => format!("{:?}", err),
            }
        }
    }
    #[test]
    fn single_binary_operation() {
        let node = parse_program(get_tokens(&"a + b;"));
        assert_eq!("(+ 0 1)", test_helpers::program_to_binop_text(&node));
    }

    #[test]
    fn double_binary_operation() {
        let node = parse_program(get_tokens(&"a + b + c;"));
        assert_eq!("(+ (+ 0 1) 2)", test_helpers::program_to_binop_text(&node));
    }

    #[test]
    fn double_binary_operation_mixed_precedence() {
        let node = parse_program(get_tokens(&"a + b * c;"));
        assert_eq!("(+ 0 (* 1 2))", test_helpers::program_to_binop_text(&node));
    }

    #[test]
    fn double_binary_operation_mixed_precedence2() {
        let node = parse_program(get_tokens(&"a * b + c;"));
        assert_eq!("(+ (* 0 1) 2)", test_helpers::program_to_binop_text(&node));
    }

    #[test]
    fn triple_binary_operation_1() {
        let node = parse_program(get_tokens(&"a + b + c + d;"));
        assert_eq!("(+ (+ (+ 0 1) 2) 3)", test_helpers::program_to_binop_text(&node));
    }

}
