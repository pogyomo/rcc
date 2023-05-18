mod input;

use std::num::ParseIntError;
use rcc_codespan::{CodeSpan, Spannable};
use rcc_token::{Token, TokenKind};
use thiserror::Error;

use crate::input::LexInput;

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
pub fn lex<S: AsRef<str>>(input: S) -> Result<Vec<Token>, Vec<LexError>> {
    let mut input = LexInput::new(input.as_ref());
    let mut token = Vec::new();
    let mut error = Vec::new();

    while {
        input.trim_whitespace();
        !input.is_eof()
    } {
        match input.token() {
            Ok(tk) => token.push(tk),
            Err(e) => error.push(e),
        }
    }

    if error.is_empty() {
        Ok(token)
    } else {
        Err(error)
    }
}

impl<'a> LexInput<'a> {
    pub fn token(&mut self) -> Result<Token, LexError> {
        let start = self.offset();
        match (self.first(), self.second()) {
            ('+', _) => {
                self.next();
                Ok(Token::new(TokenKind::Plus, CodeSpan::new(start, self.offset() - start)))
            }
            ('-', _) => {
                self.next();
                Ok(Token::new(TokenKind::Minus, CodeSpan::new(start, self.offset() - start)))
            }
            ('*', _) => {
                self.next();
                Ok(Token::new(TokenKind::Star, CodeSpan::new(start, self.offset() - start)))
            }
            ('/', _) => {
                self.next();
                Ok(Token::new(TokenKind::Slash, CodeSpan::new(start, self.offset() - start)))
            }
            ('0', 'x') => {
                self.next(); self.next();
                let value = self.parse_integer(16);
                let span = CodeSpan::new(start, self.offset() - start);
                match value {
                    Ok(value) => Ok(Token::new(TokenKind::Integer(value), span)),
                    Err(from) => Err(LexError::IntegerParseError { span, from }),
                }
            }
            ('0', 'b') => {
                self.next(); self.next();
                let value = self.parse_integer(2);
                let span = CodeSpan::new(start, self.offset() - start);
                match value {
                    Ok(value) => Ok(Token::new(TokenKind::Integer(value), span)),
                    Err(from) => Err(LexError::IntegerParseError { span, from }),
                }
            }
            ('0', _) => {
                self.next();
                let value = self.parse_integer(8);
                let span = CodeSpan::new(start, self.offset() - start);
                match value {
                    Ok(value) => Ok(Token::new(TokenKind::Integer(value), span)),
                    Err(from) => Err(LexError::IntegerParseError { span, from }),
                }
            }
            (ch, _) if ch.is_ascii_digit() => {
                let value = self.parse_integer(10);
                let span = CodeSpan::new(start, self.offset() - start);
                match value {
                    Ok(value) => Ok(Token::new(TokenKind::Integer(value), span)),
                    Err(from) => Err(LexError::IntegerParseError { span, from }),
                }
            }
            (_, _) => {
                self.next();
                let span = CodeSpan::new(start, self.offset() - start);
                Err(LexError::UnexpectedCharacter { span })
            }
        }
    }

    fn trim_whitespace(&mut self) {
        while self.first().is_ascii_whitespace() {
            self.next();
        }
    }

    fn parse_integer(&mut self, radix: u32) -> Result<u64, ParseIntError> {
        let mut body = String::new();
        while self.first().is_digit(radix) {
            body.push(self.next());
        }
        u64::from_str_radix(&body, radix)
    }
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
            Token::new(TokenKind::Integer(0o10), CodeSpan::new(13, 3)),
        ])
    }

    #[test]
    fn test_empty_input() {
        let tokens = lex("").unwrap();
        assert!(tokens.is_empty());
    }
}
