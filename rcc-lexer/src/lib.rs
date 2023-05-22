mod input;

use std::num::ParseIntError;
use rcc_codespan::{CodeSpan, Spannable};
use rcc_token::{Token, TokenKind};
use thiserror::Error;
use crate::input::LexInput;

/// An error which can be returned when lexing a string.
#[derive(Debug, Error, PartialEq, Eq)]
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
    /// Advance the input and construct a token.
    fn token(&mut self) -> Result<Token, LexError> {
        let start = self.offset();
        match (self.first(), self.second()) {
            ('+', _)   => self.consume_symbol(start, TokenKind::Plus, 1),
            ('-', _)   => self.consume_symbol(start, TokenKind::Minus, 1),
            ('*', _)   => self.consume_symbol(start, TokenKind::Star, 1),
            ('/', _)   => self.consume_symbol(start, TokenKind::Slash, 1),
            ('(', _)   => self.consume_symbol(start, TokenKind::LParen, 1),
            (')', _)   => self.consume_symbol(start, TokenKind::RParen, 1),
            ('<', '=') => self.consume_symbol(start, TokenKind::LE, 2),
            ('<', _)   => self.consume_symbol(start, TokenKind::LT, 1),
            ('>', '=') => self.consume_symbol(start, TokenKind::GE, 2),
            ('>', _)   => self.consume_symbol(start, TokenKind::GT, 1),
            ('=', '=') => self.consume_symbol(start, TokenKind::EQ, 2),
            ('=', _)   => self.consume_symbol(start, TokenKind::Assign, 1),
            (';', _)   => self.consume_symbol(start, TokenKind::Semicolon, 1),
            (',', _)   => self.consume_symbol(start, TokenKind::Comma, 1),
            ('!', '=') => self.consume_symbol(start, TokenKind::NE, 2),
            ('[', _)   => self.consume_symbol(start, TokenKind::LSquare, 1),
            (']', _)   => self.consume_symbol(start, TokenKind::RSquare, 1),
            ('{', _)   => self.consume_symbol(start, TokenKind::LCurly, 1),
            ('}', _)   => self.consume_symbol(start, TokenKind::RCurly, 1),
            ('0', 'x') => {
                self.next(); self.next();
                self.consume_integer(start, 16)
            }
            ('0', 'b') => {
                self.next(); self.next();
                self.consume_integer(start, 2)
            }
            ('0', ch) if ch.is_digit(8) => {
                self.next();
                self.consume_integer(start, 8)
            }
            (ch, _) if ch.is_ascii_digit()    => self.consume_integer(start, 10),
            (ch, _) if is_identifier_head(ch) => self.consume_identifier(start),
            (_, _) => {
                self.next();
                let span = CodeSpan::new(start, self.offset() - start);
                Err(LexError::UnexpectedCharacter { span })
            }
        }
    }

    /// Discard leading whitespace characters.
    fn trim_whitespace(&mut self) {
        while self.first().is_ascii_whitespace() {
            self.next();
        }
    }

    /// Consume given number of characters and construct symbol token.
    fn consume_symbol(&mut self, start: usize, kind: TokenKind, num: usize) -> Result<Token, LexError> {
        for _ in 0..num {
            self.next();
        }
        let span = CodeSpan::new(start, self.offset() - start);
        Ok(Token::new(kind, span))
    }

    /// Consume characters and construct integer token.
    fn consume_integer(&mut self, start: usize, radix: u32) -> Result<Token, LexError> {
        let mut body = String::new();
        while self.first().is_digit(radix) {
            body.push(self.next());
        }
        let value = u64::from_str_radix(&body, radix);
        let span = CodeSpan::new(start, self.offset() - start);
        match value {
            Ok(value) => Ok(Token::new(TokenKind::Integer(value), span)),
            Err(from) => Err(LexError::IntegerParseError { span, from }),
        }
    }

    /// Consume characters and construct identifier token.
    fn consume_identifier(&mut self, start: usize) -> Result<Token, LexError> {
        let mut body = String::new();
        while is_identifier_rest(self.first()) {
            body.push(self.next());
        }
        let span = CodeSpan::new(start, self.offset() - start);
        let kind = match body.as_str() {
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            _ => TokenKind::Identifier(body),
        };
        Ok(Token::new(kind, span))
    }
}

fn is_identifier_head(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_identifier_rest(ch: char) -> bool {
    is_identifier_head(ch) || ch.is_ascii_digit()
}

#[cfg(test)]
mod test {
    use std::assert_eq;

    use rcc_codespan::CodeSpan;
    use rcc_token::{Token, TokenKind};
    use super::lex;

    #[test]
    fn one_character_symbol() {
        let tokens = lex("+-*/<> =;,()[]{}").unwrap();
        assert_eq!(tokens, vec![
            Token::new(TokenKind::Plus,      CodeSpan::new(0, 1)),
            Token::new(TokenKind::Minus,     CodeSpan::new(1, 1)),
            Token::new(TokenKind::Star,      CodeSpan::new(2, 1)),
            Token::new(TokenKind::Slash,     CodeSpan::new(3, 1)),
            Token::new(TokenKind::LT,        CodeSpan::new(4, 1)),
            Token::new(TokenKind::GT,        CodeSpan::new(5, 1)),
            Token::new(TokenKind::Assign,    CodeSpan::new(7, 1)),
            Token::new(TokenKind::Semicolon, CodeSpan::new(8, 1)),
            Token::new(TokenKind::Comma,     CodeSpan::new(9, 1)),
            Token::new(TokenKind::LParen,    CodeSpan::new(10, 1)),
            Token::new(TokenKind::RParen,    CodeSpan::new(11, 1)),
            Token::new(TokenKind::LSquare,   CodeSpan::new(12, 1)),
            Token::new(TokenKind::RSquare,   CodeSpan::new(13, 1)),
            Token::new(TokenKind::LCurly,    CodeSpan::new(14, 1)),
            Token::new(TokenKind::RCurly,    CodeSpan::new(15, 1)),
        ])
    }

    #[test]
    fn two_character_symbol() {
        let tokens = lex("<= >= == !=").unwrap();
        assert_eq!(tokens, vec![
            Token::new(TokenKind::LE, CodeSpan::new(0, 2)),
            Token::new(TokenKind::GE, CodeSpan::new(3, 2)),
            Token::new(TokenKind::EQ, CodeSpan::new(6, 2)),
            Token::new(TokenKind::NE, CodeSpan::new(9, 2)),
        ])
    }

    #[test]
    fn identifier() {
        let tokens = lex("Hello_World L10").unwrap();
        assert_eq!(tokens, vec![
            Token::new(
                TokenKind::Identifier(String::from("Hello_World")),
                CodeSpan::new(0, 11)
            ),
            Token::new(
                TokenKind::Identifier(String::from("L10")),
                CodeSpan::new(12, 3)
            ),
        ])
    }

    #[test]
    fn integer() {
        let tokens = lex("10 0x10 0b10 010 0").unwrap();
        assert_eq!(tokens, vec![
            Token::new(TokenKind::Integer(10),   CodeSpan::new(0, 2)),
            Token::new(TokenKind::Integer(0x10), CodeSpan::new(3, 4)),
            Token::new(TokenKind::Integer(0b10), CodeSpan::new(8, 4)),
            Token::new(TokenKind::Integer(0o10), CodeSpan::new(13, 3)),
            Token::new(TokenKind::Integer(0),    CodeSpan::new(17, 1)),
        ])
    }

    #[test]
    fn keyword() {
        let tokens = lex("if else break continue").unwrap();
        assert_eq!(tokens, vec![
            Token::new(TokenKind::If,       CodeSpan::new(0, 2)),
            Token::new(TokenKind::Else,     CodeSpan::new(3, 4)),
            Token::new(TokenKind::Break,    CodeSpan::new(8, 5)),
            Token::new(TokenKind::Continue, CodeSpan::new(14, 8)),
        ]);
    }

    #[test]
    fn empty_input() {
        let tokens = lex("").unwrap();
        assert!(tokens.is_empty());
    }
}
