use derive_new::new;
use rcc_codespan::CodeSpan;

#[derive(new)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    kind: TokenKind,
    span: CodeSpan,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    Plus,
    Minus,
    Star,
    Slash,
    Integer(u64),
}
