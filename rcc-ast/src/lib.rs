use derive_new::new;
use nonempty::NonEmpty;
use rcc_codespan::{CodeSpan, Spannable};

macro_rules! impl_conv {
    ($target:ident, $($source:ident),*) => {$(
        impl From<$source> for $target {
            fn from(source: $source) -> $target {
                $target::$source(source)
            }
        }
    )*};
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Program(pub Vec<Statement>);

impl_conv!(
    Statement,
    FunctionDeclaration, VariableDeclaration, ExpressionStatement, ReturnStatement, BreakStatement,
    ContinueStatement, ForStatement, WhileStatement, IfStatement, BlockStatement
);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Statement {
    FunctionDeclaration(FunctionDeclaration),
    VariableDeclaration(VariableDeclaration),
    ExpressionStatement(ExpressionStatement),
    ReturnStatement(ReturnStatement),
    BreakStatement(BreakStatement),
    ContinueStatement(ContinueStatement),
    ForStatement(ForStatement),
    WhileStatement(WhileStatement),
    IfStatement(IfStatement),
    BlockStatement(BlockStatement),
}

impl Spannable for Statement {
    fn span(&self) -> CodeSpan {
        use Statement::*;
        match self {
            FunctionDeclaration(stmt) => stmt.span(),
            VariableDeclaration(stmt) => stmt.span(),
            ExpressionStatement(stmt) => stmt.span(),
            ReturnStatement(stmt) => stmt.span(),
            BreakStatement(stmt) => stmt.span(),
            ContinueStatement(stmt) => stmt.span(),
            ForStatement(stmt) => stmt.span(),
            WhileStatement(stmt) => stmt.span(),
            IfStatement(stmt) => stmt.span(),
            BlockStatement(stmt) => stmt.span(),
        }
    }
}

/// name(p1, p2, ..., pn) {
///     stmt1;
///     ...
///     stmtn;
/// }
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionDeclaration {
    name: FunctionDeclName,
    param: Vec<FunctionDeclParam>,
    body: BlockStatement,
}

impl Spannable for FunctionDeclaration {
    fn span(&self) -> CodeSpan {
        self.name.span() + self.body.span()
    }
}

#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionDeclName {
    name: String,
    span: CodeSpan,
}

impl FunctionDeclName {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

impl Spannable for FunctionDeclName {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionDeclParam {
    name: String,
    span: CodeSpan,
}

impl FunctionDeclParam {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

impl Spannable for FunctionDeclParam {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

/// v1, v2, ..., vn;
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VariableDeclaration {
    span: CodeSpan,
    name: NonEmpty<VariableDeclName>,
}

impl VariableDeclaration {
    pub fn new(span: CodeSpan, head: VariableDeclName, tail: Vec<VariableDeclName>) -> Self {
        Self { span, name: NonEmpty { head, tail }}
    }
}

impl Spannable for VariableDeclaration {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VariableDeclName {
    name: String,
    span: CodeSpan,
}

impl VariableDeclName {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

impl Spannable for VariableDeclName {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

/// expr;
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExpressionStatement {
    span: CodeSpan,
    expr: Expression,
}

impl Spannable for ExpressionStatement {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

/// return; or return expr;
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ReturnStatement {
    span: CodeSpan,
    expr: Option<Expression>,
}

impl Spannable for ReturnStatement {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

/// break;
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BreakStatement {
    span: CodeSpan,
}

impl Spannable for BreakStatement {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

/// continue;
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ContinueStatement {
    span: CodeSpan,
}

impl Spannable for ContinueStatement {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

/// for (init; cond; update) {
///     stmt1;
///     ...
///     stmtn;
/// }
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ForStatement {
    span: CodeSpan,
    init_expr: Option<Expression>,
    cond_expr: Option<Expression>,
    update_expr: Option<Expression>,
    body: BlockStatement,
}

impl Spannable for ForStatement {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

/// while (cond) {
///     stmt1;
///     ...
///     stmtn;
/// }
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct WhileStatement {
    span: CodeSpan,
    cond_expr: Expression,
    body: BlockStatement,
}

impl Spannable for WhileStatement {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

/// if (cond) {
///     stmt1;
///     ...
///     stmtn;
/// } else {
///     stmt1;
///     ...
///     stmtn;
/// }
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IfStatement {
    span: CodeSpan,
    cond_expr: Expression,
    body: BlockStatement,
    else_body: Option<BlockStatement>
}

impl Spannable for IfStatement {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

/// {
///     stmt1;
///     ...
///     stmtn;
/// }
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockStatement {
    span: CodeSpan,
    body: Vec<Statement>,
}

impl Spannable for BlockStatement {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

impl_conv!(
    Expression,
    IntegerExpression, VariableExpression, VariableAssignment, FunctionCall, PrefixExpression,
    InfixExpression
);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    IntegerExpression(IntegerExpression),
    VariableExpression(VariableExpression),
    VariableAssignment(VariableAssignment),
    FunctionCall(FunctionCall),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
}

impl Spannable for Expression {
    fn span(&self) -> CodeSpan {
        use Expression::*;
        match self {
            IntegerExpression(expr) => expr.span(),
            VariableExpression(expr) => expr.span(),
            VariableAssignment(expr) => expr.span(),
            FunctionCall(expr) => expr.span(),
            PrefixExpression(expr) => expr.span(),
            InfixExpression(expr) => expr.span(),
        }
    }
}

/// 100, 0x100
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntegerExpression {
    span: CodeSpan,
    value: u64,
}

impl Spannable for IntegerExpression {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

/// name
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VariableExpression {
    span: CodeSpan,
    name: String,
}

impl Spannable for VariableExpression {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

/// lhs = rhs
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VariableAssignment {
    lhs: Box<Expression>,
    rhs: Box<Expression>,
}

impl Spannable for VariableAssignment {
    fn span(&self) -> CodeSpan {
        self.lhs.span() + self.rhs.span()
    }
}

/// name(p1, ..., pn)
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCall {
    name: FunctionCallName,
    param: Vec<Expression>,
}

impl Spannable for FunctionCall {
    fn span(&self) -> CodeSpan {
        let mut span = self.name.span();
        for param in self.param.iter() {
            span += param.span();
        }
        span
    }
}

#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionCallName {
    span: CodeSpan,
    name: String,
}

impl Spannable for FunctionCallName {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

/// op expr
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PrefixExpression {
    span: CodeSpan,
    op: PrefixOp,
    expr: Box<Expression>,
}

impl Spannable for PrefixExpression {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrefixOp {
    /// "-"
    Negation,
}

/// lhs op rhs
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InfixExpression {
    op: InfixOp,
    lhs: Box<Expression>,
    rhs: Box<Expression>,
}

impl Spannable for InfixExpression {
    fn span(&self) -> CodeSpan {
        self.lhs.span() + self.rhs.span()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum InfixOp {
    /// "+"
    Add,
    /// "-"
    Sub,
    /// "*"
    Mul,
    /// "/"
    Div,
    /// "<"
    LT,
    /// ">"
    GT,
    /// "<="
    LE,
    /// ">="
    GE,
    /// "=="
    EQ,
    /// "!="
    NE,
}
