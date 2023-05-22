use derive_new::new;
use rcc_codespan::{Spannable, CodeSpan};

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    pub kind: TokenKind,
    span: CodeSpan,
}

impl Spannable for Token {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    // One-character token
    /// "+"
    Plus,
    /// "-"
    Minus,
    /// "*"
    Star,
    /// "/"
    Slash,
    /// "<"
    LT,
    /// ">"
    GT,
    /// "="
    Assign,
    /// ";"
    Semicolon,
    /// ","
    Comma,
    /// "("
    LParen,
    /// ")"
    RParen,
    /// "["
    LSquare,
    /// "]"
    RSquare,
    /// "{"
    LCurly,
    /// "}"
    RCurly,

    // Two-character token
    /// "<="
    LE,
    /// ">="
    GE,
    /// "=="
    EQ,
    /// "!="
    NE,

    // Literal
    /// "10", "0x10", "0b10", "010"
    Integer(u64),
    /// "hello_world", "Ident", "L10"
    Identifier(String),

    // Keyword
    /// if
    If,
    /// else
    Else,
    /// break
    Break,
    /// continue
    Continue,
}
