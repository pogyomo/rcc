use std::{rc::Rc, todo};
use nonempty::NonEmpty;
use rcc_ast::{Program, Statement, IfStatement, BreakStatement, Expression, IntegerExpression, ContinueStatement};
use rcc_codespan::{Spannable, CodeSpan};
use rcc_token::{Token, TokenKind};
use thiserror::Error;

use crate::stack::TokenStack;

mod stack;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ParseError {
    #[error("expect {expect}, but got {got}")]
    UnexpectedTokenFound {
        expect: &'static str,
        got: String,
        span: CodeSpan,
    },
    #[error("expect {expect} after the token, but not found")]
    ExpectTokenButNotFound {
        expect: &'static str,
        last: CodeSpan,
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Parser {
    tokens: Vec<Token>
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    pub fn parse(self) -> Result<Program, ParseError> {
        let tokens = self.tokens.into_iter().map(|v| Rc::new(v)).collect();
        match NonEmpty::from_vec(tokens) {
            Some(vec) => NonEmptyParser::new(vec).parse(),
            None => Ok(Program(Vec::new()))
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
struct NonEmptyParser {
    tokens: NonEmpty<Rc<Token>>,
    position: usize,
}

impl NonEmptyParser {
    pub fn new(tokens: NonEmpty<Rc<Token>>) -> Self {
        Self { tokens, position: 0 }
    }

    pub fn parse(mut self) -> Result<Program, ParseError> {
        let mut program = Vec::new();
        while !self.is_empty() {
            program.push(self.parse_stmt()?);
        }
        Ok(Program(program))
    }
}

impl NonEmptyParser {
    fn parse_stmt(&mut self) -> Result<Statement, ParseError> {
        let stack = TokenStack::new(self.peek_or_err("if or break")?);
        match &stack.inspect().kind {
            TokenKind::If => self.parse_if(),
            TokenKind::Break => self.parse_break(),
            TokenKind::Continue => self.parse_continue(),
            _ => todo!(),
        }
    }

    fn parse_if(&mut self) -> Result<Statement, ParseError> {
        let mut stack = TokenStack::new(self.next_or_err("if")?);
        stack.push(self.next_or_err("(")?);
        match &stack.inspect().kind {
            TokenKind::LParen => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: "(",
                got: String::new(), // TODO
                span: stack.inspect().span(),
            })
        }

        let cond = self.parse_expr()?;

        stack.push(self.next_or_err(")")?);
        match &stack.inspect().kind {
            TokenKind::RParen => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: ")",
                got: String::new(), // TODO
                span: stack.inspect().span(),
            })
        }

        stack.push(self.next_or_err("{")?);
        match &stack.inspect().kind {
            TokenKind::LCurly => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: "{",
                got: String::new(), // TODO
                span: stack.inspect().span(),
            })
        }

        let mut body = Vec::new();
        loop {
            stack.push(self.peek_or_err("statement or }")?);
            match &stack.inspect().kind {
                TokenKind::RCurly => {
                    self.next();
                    break;
                }
                _ => (),
            }
            body.push(self.parse_stmt()?);
        }

        let Some(token) = self.next() else {
            return Ok(Statement::IfStatement(IfStatement::new(
                stack.span(), cond, body, None
            )))
        };

        stack.push(token);
        match &stack.inspect().kind {
            TokenKind::Else => (),
            _ => return Ok(Statement::IfStatement(IfStatement::new(
                stack.span(), cond, body, None
            )))
        }

        stack.push(self.next_or_err("{")?);
        match &stack.inspect().kind {
            TokenKind::LCurly => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: "{",
                got: String::new(), // TODO
                span: stack.inspect().span(),
            })
        }

        let mut else_stmt = Vec::new();
        loop {
            stack.push(self.peek_or_err("statement or }")?);
            match &stack.inspect().kind {
                TokenKind::RCurly => {
                    self.next();
                    break;
                }
                _ => (),
            }
            else_stmt.push(self.parse_stmt()?);
        }

        Ok(Statement::IfStatement(IfStatement::new(
            stack.span(), cond, body, Some(else_stmt)
        )))
    }

    fn parse_break(&mut self) -> Result<Statement, ParseError> {
        let mut stack = TokenStack::new(self.next_or_err("break")?);
        match &stack.inspect().kind {
            TokenKind::Break => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: "break",
                got: String::new(),
                span: stack.inspect().span(),
            })
        }
        stack.push(self.next_or_err(";")?);
        match &stack.inspect().kind {
            TokenKind::Semicolon => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: ";",
                got: String::new(),
                span: stack.inspect().span(),
            })
        }
        Ok(Statement::BreakStatement(BreakStatement::new(stack.span())))
    }

    fn parse_continue(&mut self) -> Result<Statement, ParseError> {
        let mut stack = TokenStack::new(self.next_or_err("continue")?);
        match &stack.inspect().kind {
            TokenKind::Continue => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: "continue",
                got: String::new(),
                span: stack.inspect().span(),
            })
        }
        stack.push(self.next_or_err(";")?);
        match &stack.inspect().kind {
            TokenKind::Semicolon => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: ";",
                got: String::new(),
                span: stack.inspect().span(),
            })
        }
        Ok(Statement::ContinueStatement(ContinueStatement::new(stack.span())))
    }
}

impl NonEmptyParser {
    fn parse_expr(&mut self) -> Result<Expression, ParseError> {
        let stack = TokenStack::new(self.next_or_err("integer")?);
        match &stack.inspect().kind {
            TokenKind::Integer(value) => Ok(
                Expression::IntegerExpression(
                    IntegerExpression::new(stack.span(), *value)
                )
            ),
            _ => todo!()
        }
    }
}

impl NonEmptyParser {
    fn is_empty(&self) -> bool {
        self.tokens.get(self.position).is_none()
    }

    fn peek(&mut self) -> Option<Rc<Token>> {
        self.tokens.get(self.position).map(|v| Rc::clone(v))
    }

    fn peek_or_err(&mut self, expect: &'static str) -> Result<Rc<Token>, ParseError> {
        self.peek().ok_or(ParseError::ExpectTokenButNotFound {
            expect, last: self.tokens.last().span()
        })
    }

    fn next(&mut self) -> Option<Rc<Token>> {
        let next = self.tokens.get(self.position);
        self.position += 1;
        next.map(|v| Rc::clone(v))
    }

    fn next_or_err(&mut self, expect: &'static str) -> Result<Rc<Token>, ParseError> {
        self.next().ok_or(ParseError::ExpectTokenButNotFound {
            expect, last: self.tokens.last().span()
        })
    }
}
