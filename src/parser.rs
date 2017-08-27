use string_table::StringIndex;

struct CallExpressionData {
    pub callee: StringIndex,
    pub args: Box<SyntaxNode>
}

struct PrototypeData {
    pub name: StringIndex,
    pub args: Vec<StringIndex>
}

struct BinaryExpressionData {
    pub operation: StringIndex,
    pub lhs: Box<SyntaxNode>,
    pub rhs:, Box<SyntaxNode>,
}

pub enum SyntaxNode {
    Number(f64),
    Variable(StringIndex),
    BinaryExpression(BinaryExpressionData),
    CallExpression(CallExpressionData),
    Prototype(PrototypeData),
}
