use string_table::StringIndex;
use tokens::Token;
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
pub struct ReturnData {
    pub expression: Box<SyntaxNode>
}

#[derive(Debug,PartialEq,Clone)]
pub struct BinaryExpressionData {
    pub operation: char,
    pub lhs: Box<SyntaxNode>,
    pub rhs: Box<SyntaxNode>,
}

#[derive(Debug,PartialEq,Clone)]
pub struct FunctionData {
    pub name: StringIndex,
    pub args: Vec<StringIndex>,
    pub body: Vec<SyntaxNode>,
}

#[derive(Debug,PartialEq,Clone)]
pub struct IfElseData {
    pub condition: Box<SyntaxNode>,
    pub when_true: Vec<SyntaxNode>,
    pub when_false: Vec<SyntaxNode>,
}

#[derive(Debug,PartialEq,Clone)]
pub enum SyntaxNode {
    Program(ProgramData),
    Number(f64),
    Variable(StringIndex),
    BinaryExpression(BinaryExpressionData),
    CallExpression(CallExpressionData),
    Function(FunctionData),
    Return(ReturnData),
    IfElse(IfElseData)
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
    NotImplemented,
    UnmatchedParen,
    UnknownArgumentValue,
    NotExpressionToken,
    NoArgIdentifier,
    ExpectedOpeningParens,
    ExpectedClosingParens,
    ExpectedFunctionName,
    ExpectedOpeningCurlyBrace,
    ExpectedClosingCurlyBrace,
    UnknownCharacterInProgram,
    ExpectedTerm,
    UnmatchedParens,
    UnexpectedTokenInFunctionBlock,
    UnmatchedCurlyBrace,
    UnexpectedElse,
}

type PeekableTokens = Peekable<IntoIter<Token>>;
type ResultNode = Result<SyntaxNode, ParseError>;

// FUNCTION -> "function" (identifier, ...) {
//               BLOCK
//               RETURN_EXPRESSION
//             }
//             e.g. function () { return 0; }
//             e.g. funtion (a, b) { a = a + b; return a }
fn parse_function (mut tokens: &mut PeekableTokens) -> ResultNode {
    // function foo(bar, baz) { }
    // ^
    tokens.next();

    // function foo(bar, baz) { }
    //          ^
    let name = match tokens.next() {
        Some(Token::Identifier(string_index)) => string_index,
        _ => return Err(ParseError::ExpectedFunctionName)
    };

    // function foo(bar, baz) { }
    //             ^
    match tokens.next() {
        Some(Token::Char('(')) => {},
        _ => return Err(ParseError::ExpectedOpeningParens)
    };

    let mut args = Vec::new();

    // Process function args
    loop {
        match tokens.next() {
            // function foo() {}
            //              ^
            Some(Token::Char(')')) => { break },
            // function foo(bar, baz) {}
            //              ^    ^
            Some(Token::Identifier(arg_name)) => { args.push(arg_name) },
            _ => return Err(ParseError::UnknownArgumentValue)
        }

        match tokens.next() {
            // function foo(bar, baz) {}
            //                      ^
            Some(Token::Char(')')) => { break },
            // function foo(bar, baz) {}
            //                 ^
            Some(Token::Char(',')) => continue,
            _ => return Err(ParseError::UnknownArgumentValue)
        }
    }

    Ok(SyntaxNode::Function(FunctionData {
        name: name,
        args: args,
        body: parse_block(&mut tokens)?,
    }))
}

macro_rules! common_block_matchers {
    () => {

    }
}

fn parse_function_call(
    mut tokens: &mut PeekableTokens
) -> Result<Vec<SyntaxNode>, ParseError> {
    // function foo(bar, baz)
    //             ^
    match tokens.next() {
        Some(Token::Char('(')) => {},
        _ => return Err(ParseError::ExpectedOpeningParens)
    };

    let mut args = Vec::new();

    match tokens.peek() {
        // function foo()
        //              ^
        Some(&Token::Char(')')) => {},
        _ => {
            // Process function args
            loop {
                args.push(parse_expression(&mut tokens)?);

                match tokens.next() {
                    // function foo(bar, baz) {}
                    //                      ^
                    Some(Token::Char(')')) => { break },
                    // function foo(bar, baz) {}
                    //                 ^
                    Some(Token::Char(',')) => { continue },
                    _ => return Err(ParseError::UnknownArgumentValue)
                }
            }
        }
    }
    // Skip ')'
    tokens.next();
    Ok(args)
}

fn parse_block (
    mut tokens: &mut PeekableTokens
) -> Result<Vec<SyntaxNode>, ParseError> {

    match tokens.next() {
        Some(Token::Char('{')) => {},
        _ => { return Err(ParseError::ExpectedOpeningCurlyBrace) }
    };

    let mut body = Vec::new();
    loop {
        match tokens.peek() {
            // Skip any semi-colons.
            Some(&Token::Char(';')) => {
                tokens.next();
            },
            // This is some kind of identifier or number, parse it as an expression.
            Some(&Token::Identifier(_))
            | Some(&Token::Number(_)) => {
                body.push(parse_expression(&mut tokens)?);
            },
            Some(&Token::Char('}')) => {
                tokens.next();
                return Ok(body);
            },
            Some(&Token::If) => {
                body.push(parse_if_else(&mut tokens)?);
            },
            Some(&Token::Return) => {
                tokens.next();
                body.push(SyntaxNode::Return(ReturnData {
                    expression: Box::new(parse_expression(&mut tokens)?)
                }));
            },
            Some(_) => {
                return Err(ParseError::UnexpectedTokenInFunctionBlock);
            },
            None => {
                return Err(ParseError::UnmatchedCurlyBrace);
            },
        }
    }
}

fn parse_if_else (mut tokens: &mut PeekableTokens) -> ResultNode {
    // Skip "if"
    tokens.next();
    // '('
    match tokens.next() {
        Some(Token::Char('(')) => {},
        _ => { return Err(ParseError::ExpectedOpeningParens); },
    }

    let condition = parse_parens_expression(&mut tokens)?;

    let when_true = parse_block(&mut tokens)?;

    let when_false = match tokens.peek() {
        Some(&Token::Else) => {
            tokens.next();
            parse_block(&mut tokens)?
        },
        _ => Vec::new(),
    };

    Ok(SyntaxNode::IfElse(IfElseData {
        condition: Box::new(condition),
        when_true: when_true,
        when_false: when_false,
    }))
}

// TERM -> VARIABLE
//       | NUMBER
fn parse_term (mut tokens: &mut PeekableTokens) -> ResultNode {
    match tokens.next() {
        Some(Token::Identifier(string_index)) => {
            match tokens.peek() {
                Some(&Token::Char('(')) => {
                    Ok(SyntaxNode::CallExpression(CallExpressionData {
                        callee: string_index,
                        args: parse_function_call(&mut tokens)?,
                    }))
                },
                _ => Ok(SyntaxNode::Variable(string_index)),
            }
        },
        Some(Token::Number(number)) => Ok(SyntaxNode::Number(number)),
        Some(Token::Char('(')) => parse_parens_expression(&mut tokens),
        _ => Err(ParseError::ExpectedTerm),
    }
}

fn parse_parens_expression(mut tokens: &mut PeekableTokens) -> ResultNode {
    let expression = parse_expression(&mut tokens)?;
    match tokens.next() {
        Some(Token::Char(')')) => Ok(expression),
        _ => Err(ParseError::UnmatchedParens),
    }
}

// EXPRESSION -> VARIABLE
//             | NUMBER
//             | EXPRESSION BINOP EXPRESSION
fn parse_expression(mut tokens: &mut PeekableTokens) -> Result<SyntaxNode, ParseError> {
    let mut term = parse_term(&mut tokens)?;
    loop {
        term = parse_binary_operation_rhs(&mut tokens, 0, term)?;
        if get_operator_precedence(tokens.peek()) == -1 {
            return Ok(term);
        }
    }
}

fn parse_binary_operation_rhs(
    mut tokens: &mut PeekableTokens,
    lhs_precedence: i32,
    lhs: SyntaxNode
) -> Result<SyntaxNode, ParseError> {
    // The operator precedence is -1 if it's not an operator.
    let operator_precedence = get_operator_precedence(tokens.peek());
    if operator_precedence < lhs_precedence {
        return Ok(lhs);
    }

    // This is a binary operation, skip over the token.
    let operation_char = match tokens.next() {
        Some(Token::Char(op_char)) => op_char,
        _ => panic!("This should be an operation character.")
    };

    // Parse the rhs expression
    let mut rhs = parse_term(&mut tokens)?;

    // If the binary operation is stronger, swap out the result recursively.
    if operator_precedence < get_operator_precedence(tokens.peek()) {
        rhs = parse_binary_operation_rhs(&mut tokens, operator_precedence, rhs)?;
    }

    Ok(SyntaxNode::BinaryExpression(BinaryExpressionData {
        operation: operation_char,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }))
}


fn get_operator_precedence(token: Option<&Token>) -> i32 {
    match token {
        Some(&Token::Char('<')) => 1,
        Some(&Token::Char('>')) => 1,
        Some(&Token::Char('+')) => 2,
        Some(&Token::Char('-')) => 2,
        Some(&Token::Char('*')) => 3,
        Some(&Token::Char('/')) => 3,
        _ => -1,
    }
}

// PROGRAM -> FUNCTION    e.g. function foo( ) {}
//         |  EXPRESSION  e.g. x + y
pub fn parse_program (tokens_vec: Vec<Token>) -> ResultNode {
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
            | Some(&Token::Number(_))
            | Some(&Token::If)
            | Some(&Token::Return)
            => {
                body.push(parse_expression(&mut tokens)?);
            },
            // This is an unexpected character, error out.
            Some(&Token::Char(_)) => {
                return Err(ParseError::UnknownCharacterInProgram);
            },
            Some(&Token::Else) => {
                return Err(ParseError::UnexpectedElse)
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

        #[allow(dead_code)]
        pub fn get_program_body_node(result: Result<SyntaxNode, ParseError>) -> SyntaxNode {
            match result {
                Ok(SyntaxNode::Program(ProgramData { body })) => {
                    if body.len() != 1 {
                        panic!("There is more than one body node in the program {:?}", body);
                    }
                    return body.get(0).unwrap().clone();
                },
                Ok(node) => {
                    panic!("Unknown root node {:?}", node);
                },
                Err(err) => {
                    panic!("Unexpected parse error {:?}", err);
                }
            }
        }

        pub fn get_program_body(result: Result<SyntaxNode, ParseError>) -> Vec<SyntaxNode> {
            match result {
                Ok(SyntaxNode::Program(ProgramData { body })) => {
                    return body;
                },
                Ok(node) => {
                    panic!("Unknown root node {:?}", node);
                },
                Err(err) => {
                    panic!("Unexpected parse error {:?}", err);
                }
            }
        }
    }

    mod binary_operations {
        use super::*;
        #[test]
        fn single() {
            let result = parse_program(get_tokens(&"a + b;"));
            assert_eq!("(+ 0 1)", test_helpers::program_to_binop_text(&result));
        }

        #[test]
        fn double() {
            let result = parse_program(get_tokens(&"a + b + c;"));
            assert_eq!("(+ (+ 0 1) 2)", test_helpers::program_to_binop_text(&result));
        }

        #[test]
        fn double_mixed_precedence_1() {
            let result = parse_program(get_tokens(&"a + b * c;"));
            assert_eq!("(+ 0 (* 1 2))", test_helpers::program_to_binop_text(&result));
        }

        #[test]
        fn double_mixed_precedence_2() {
            let result = parse_program(get_tokens(&"a * b + c;"));
            assert_eq!("(+ (* 0 1) 2)", test_helpers::program_to_binop_text(&result));
        }

        #[test]
        fn triple() {
            let result = parse_program(get_tokens(&"a + b + c + d;"));
            assert_eq!("(+ (+ (+ 0 1) 2) 3)", test_helpers::program_to_binop_text(&result));
        }

        #[test]
        fn triple_mixed_precedence() {
            let result = parse_program(get_tokens(&"a * b + c * d;"));
            assert_eq!("(+ (* 0 1) (* 2 3))", test_helpers::program_to_binop_text(&result));
        }

        #[test]
        fn double_parenthesis() {
            let result = parse_program(get_tokens(&"a + (b + c);"));
            assert_eq!("(+ 0 (+ 1 2))", test_helpers::program_to_binop_text(&result));
        }

        #[test]
        fn unmatched_parens() {
            let result = parse_program(get_tokens(&"a + (b + c"));
            assert_eq!(Err(ParseError::UnmatchedParens), result);
        }
    }

    mod function_definition {
        use super::*;
        #[test]
        fn simple_function() {
            let tokens = get_tokens(&"function foo() {}");
            let result = parse_program(tokens);
            assert_eq!(
                test_helpers::get_program_body_node(result),
                SyntaxNode::Function(FunctionData {
                    name: 0,
                    args: Vec::new(),
                    body: Vec::new()
                })
            );
        }

        #[test]
        fn single_arg_function() {
            let tokens = get_tokens(&"function foo(a) {}");
            let result = parse_program(tokens);
            assert_eq!(
                test_helpers::get_program_body_node(result),
                SyntaxNode::Function(FunctionData {
                    name: 0,
                    args: vec![1],
                    body: Vec::new()
                })
            );
        }

        #[test]
        fn multi_arg_function() {
            let tokens = get_tokens(&"function foo(a, b, c) {}");
            let result = parse_program(tokens);
            assert_eq!(
                test_helpers::get_program_body_node(result),
                SyntaxNode::Function(FunctionData {
                    name: 0,
                    args: vec![1, 2, 3],
                    body: Vec::new()
                })
            );
        }

        #[test]
        fn function_with_body() {
            let tokens = get_tokens(&"
                function foo() {
                    return 1;
                }
            ");
            let result = parse_program(tokens);
            assert_eq!(
                test_helpers::get_program_body_node(result),
                SyntaxNode::Function(FunctionData {
                    name: 0,
                    args: vec![],
                    body: vec![
                        SyntaxNode::Return(ReturnData {
                            expression: Box::new(SyntaxNode::Number(1.0))
                        })
                    ]
                })
            );
        }

        #[test]
        fn function_with_if_else() {
            let tokens = get_tokens(&"
                function foo() {
                    if (1) {
                        return 1;
                    } else {
                        return 0;
                    }
                }
            ");
            let result = parse_program(tokens);
            assert_eq!(
                test_helpers::get_program_body_node(result),
                SyntaxNode::Function(FunctionData {
                    name: 0,
                    args: vec![],
                    body: vec![SyntaxNode::IfElse(IfElseData {
                        condition: Box::new(SyntaxNode::Number(1.0)),
                        when_true: vec![SyntaxNode::Return(ReturnData {
                            expression: Box::new(SyntaxNode::Number(1.0))
                        })],
                        when_false: vec![SyntaxNode::Return(ReturnData {
                            expression: Box::new(SyntaxNode::Number(0.0))
                        })]
                    })]
                })
            );
        }
    }

    mod full_test {
        use super::*;

        #[test]
        fn test_full() {
            let tokens = get_tokens(&"
                // Compute the x'th fibonacci number.
                function fib(x) {
                  if (x < 3.0) {
                    return 1;
                  } else {
                    return fib(x-1) + fib(x-2);
                  }
                }

                // This expression will compute the 40th number.
                fib(40);
            ");
            let result = parse_program(tokens);
            println!("{:#?}", result);
        }
    }
}
