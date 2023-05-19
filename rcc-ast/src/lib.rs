use derive_new::new;
use nonempty::NonEmpty;
use rcc_codespan::{CodeSpan, Spannable};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Statement {
    FunctionDeclaration(FunctionDeclaration),
    VariableDeclaration(VariableDeclaration),
    VariableAssignment(VariableAssignment),
    ReturnStatement(ReturnStatement),
    BreakStatement(BreakStatement),
    ContinueStatement(ContinueStatement),
    ForStatement(ForStatement),
    WhileStatement(WhileStatement),
    IfStatement(IfStatement),
}

impl Spannable for Statement {
    fn span(&self) -> CodeSpan {
        use Statement::*;
        match self {
            FunctionDeclaration(stmt) => stmt.span(),
            VariableDeclaration(stmt) => stmt.span(),
            VariableAssignment(stmt) => stmt.span(),
            ReturnStatement(stmt) => stmt.span(),
            BreakStatement(stmt) => stmt.span(),
            ContinueStatement(stmt) => stmt.span(),
            ForStatement(stmt) => stmt.span(),
            WhileStatement(stmt) => stmt.span(),
            IfStatement(stmt) => stmt.span(),
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
    body: Vec<Statement>,
}

impl Spannable for FunctionDeclaration {
    fn span(&self) -> CodeSpan {
        let mut span = self.name.span();
        for param in self.param.iter() {
            span += param.span();
        }
        for stmt in self.body.iter() {
            span += stmt.span();
        }
        span
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
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VariableDeclaration {
    name: NonEmpty<VariableDeclName>,
}

impl Spannable for VariableDeclaration {
    fn span(&self) -> CodeSpan {
        let mut span = self.name.first().span();
        for name in self.name.iter() {
            span += name.span();
        }
        span
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

/// name = expr;
#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VariableAssignment {
    name: VariableAssignName,
    expr: Expression,
}

impl Spannable for VariableAssignment {
    fn span(&self) -> CodeSpan {
        self.name.span() + self.expr.span()
    }
}

#[derive(new, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VariableAssignName {
    name: String,
    span: CodeSpan,
}

impl VariableAssignName {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

impl Spannable for VariableAssignName {
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
    body: Vec<Statement>,
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
    body: Vec<Statement>,
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
    body: Vec<Statement>,
    else_stmt: Option<Vec<Statement>>
}

impl Spannable for IfStatement {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    // TODO
}

impl Spannable for Expression {
    fn span(&self) -> CodeSpan {
        todo!()
    }
}
