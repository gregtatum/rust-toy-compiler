# Rust Toy Compiler (WIP)

This project is an exploration into building a simple compiler in Rust using LLVM. It's somewhat based off of the [Kalaidescope LLVM tutorial](https://llvm.org/docs/tutorial/).

## Run the example

```
cargo run --example read-file examples/fibonacci.toy
```

It takes in this text in a pseudo-language.

```javascript
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
```

It will then generate a list of tokens:

```
[Function, Identifier(0), Char('('), Identifier(1), Char(')'), Char('{'), If, Char('('), Identifier(1), Char('<'), Number(3), Char(')'), Char('{'), Return, Number(1), Char(';'), Char('}'), Else, Char('{'), Return, Identifier(0), Char('('), Identifier(1), Char('-'), Number(1), Char(')'), Char('+'), Identifier(0), Char('('), Identifier(1), Char('-'), Number(2), Char(')'), Char(';'), Char('}'), Char('}'), Identifier(0), Char('('), Number(40), Char(')'), Char(';')]
```

And finally compute an AST.

```
Ok(
    Program(
        ProgramData {
            body: [
                Function(
                    FunctionData {
                        name: 0,
                        args: [
                            1
                        ],
                        body: [
                            IfElse(
                                IfElseData {
                                    condition: BinaryExpression(
                                        BinaryExpressionData {
                                            operation: '<',
                                            lhs: Variable(
                                                1
                                            ),
                                            rhs: Number(
                                                3
                                            )
                                        }
                                    ),
                                    when_true: [
                                        Return(
                                            ReturnData {
                                                expression: Number(
                                                    1
                                                )
                                            }
                                        )
                                    ],
...
etc.
```
