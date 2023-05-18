use derive_new::new;
use rcc_codespan::{Spannable, CodeSpan};

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    kind: TokenKind,
    span: CodeSpan,
}

impl Spannable for Token {
    fn span(&self) -> CodeSpan {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    Plus,
    Minus,
    Star,
    Slash,
    Integer(u64),
    Identifier(String),
}
