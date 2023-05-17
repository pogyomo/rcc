use std::num::ParseIntError;

use rcc_codespan::{CodeSpan, Spannable};
use rcc_token::Token;
use thiserror::Error;

/// An error which can be returned when lexing a string.
#[derive(Debug, Error)]
pub enum LexError {
    #[error("unexpected character found")]
    UnexpectedCharacter{ span: CodeSpan },
    #[error("failed to parse integer: {from}")]
    IntegerParseError{ span: CodeSpan, from: ParseIntError }
}

impl Spannable for LexError {
    fn span(&self) -> CodeSpan {
        match *self {
            LexError::UnexpectedCharacter{ span } => span,
            LexError::IntegerParseError{ span, .. } => span,
        }
    }
}

/// Lex given input and generate token list, or return list of error if there exist problems.
pub fn lex<S: AsRef<str>>(_input: S) -> Result<Vec<Token>, Vec<LexError>> {
    todo!()
}

#[cfg(test)]
mod test {
    use rcc_codespan::CodeSpan;
    use rcc_token::{Token, TokenKind};
    use super::lex;

    #[test]
    fn test_symbol() {
        let tokens = lex("+-*/").unwrap();
        assert_eq!(tokens, vec![
            Token::new(TokenKind::Plus,  CodeSpan::new(0, 1)),
            Token::new(TokenKind::Minus, CodeSpan::new(1, 1)),
            Token::new(TokenKind::Star,  CodeSpan::new(2, 1)),
            Token::new(TokenKind::Slash, CodeSpan::new(3, 1)),
        ])
    }

    #[test]
    fn test_integer() {
        let tokens = lex("10 0x10 0b10 010").unwrap();
        assert_eq!(tokens, vec![
            Token::new(TokenKind::Integer(10),   CodeSpan::new(0, 2)),
            Token::new(TokenKind::Integer(0x10), CodeSpan::new(3, 4)),
            Token::new(TokenKind::Integer(0b10), CodeSpan::new(8, 4)),
            Token::new(TokenKind::Integer(010),  CodeSpan::new(13, 3)),
        ])
    }
}
