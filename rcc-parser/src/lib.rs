use std::rc::Rc;
use nonempty::NonEmpty;
use rcc_ast::{
    Program, Statement, IfStatement, BreakStatement, Expression, IntegerExpression, ContinueStatement,
    ReturnStatement, BlockStatement, ForStatement, WhileStatement, ExpressionStatement, FunctionDeclaration,
    FunctionDeclName, FunctionDeclParam, VariableDeclaration, VariableDeclName, PrefixExpression, PrefixOp,
    InfixExpression, InfixOp, VariableAssignment, VariableExpression
};
use rcc_codespan::{Spannable, CodeSpan};
use rcc_token::{Token, TokenKind};
use thiserror::Error;
use crate::stack::TokenStack;

mod stack;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ParseError {
    #[error("expect {expect}, but got the token")]
    UnexpectedTokenFound {
        expect: &'static str,
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
            TokenKind::If => Ok(self.parse_if()?.into()),
            TokenKind::Break => Ok(self.parse_break()?.into()),
            TokenKind::Continue => Ok(self.parse_continue()?.into()),
            TokenKind::Return => Ok(self.parse_return()?.into()),
            TokenKind::For => Ok(self.parse_for()?.into()),
            TokenKind::While => Ok(self.parse_while()?.into()),
            TokenKind::LCurly => Ok(self.parse_block()?.into()),
            TokenKind::Identifier(_) => {
                let position = self.position;
                self.next();
                match self.peek_or_err(";, ( or ,")?.kind {
                    TokenKind::Comma | TokenKind::Semicolon => {
                        self.position = position;
                        Ok(self.parse_var_decl()?.into())
                    }
                    TokenKind::LParen => {
                        self.position = position;
                        Ok(self.parse_func_decl()?.into())
                    }
                    _ => {
                        self.position = position;
                        Ok(self.parse_expr_stmt()?.into())
                    }
                }
            }
            _ => Ok(self.parse_expr_stmt()?.into()),
        }
    }

    fn parse_func_decl(&mut self) -> Result<FunctionDeclaration, ParseError> {
        let mut stack = TokenStack::new(self.next_or_err("identifier")?);
        let name = match &stack.inspect().kind {
            TokenKind::Identifier(s) => FunctionDeclName::new(s.clone(), stack.inspect().span()),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: "identifier",
                span: stack.inspect().span(),
            })
        };

        stack.push(self.next_or_err("(")?);
        if !matches!(stack.inspect().kind, TokenKind::LParen) {
            return Err(ParseError::UnexpectedTokenFound {
                expect: "(",
                span: stack.inspect().span(),
            })
        }

        let mut param = Vec::new();
        loop {
            stack.push(self.next_or_err("identifier or )")?);
            match &stack.inspect().kind {
                TokenKind::Identifier(s) => {
                    param.push(FunctionDeclParam::new(s.clone(), stack.inspect().span()));
                    stack.push(self.next_or_err(", or )")?);
                    match &stack.inspect().kind {
                        TokenKind::Comma => (),
                        TokenKind::RParen => break,
                        _ => return Err(ParseError::UnexpectedTokenFound {
                            expect: ", or )",
                            span: stack.inspect().span()
                        })
                    }
                }
                TokenKind::RParen => break,
                _ => return Err(ParseError::UnexpectedTokenFound {
                    expect: "identifier or )",
                    span: stack.inspect().span()
                })
            }
        }

        let body = self.parse_block()?;

        Ok(FunctionDeclaration::new(name, param, body))
    }

    fn parse_var_decl(&mut self) -> Result<VariableDeclaration, ParseError> {
        let mut stack = TokenStack::new(self.next_or_err("identifier")?);
        let var1 = match &stack.inspect().kind {
            TokenKind::Identifier(s) => VariableDeclName::new(s.clone(), stack.inspect().span()),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: "identifier",
                span: stack.inspect().span(),
            })
        };

        stack.push(self.next_or_err(", or ;")?);
        match &stack.inspect().kind {
            TokenKind::Comma => (),
            TokenKind::Semicolon => return Ok(VariableDeclaration::new(stack.span(), var1, Vec::new())),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: ", or ;",
                span: stack.inspect().span(),
            })
        }

        let mut vars = Vec::new();
        loop {
            stack.push(self.next_or_err("identifier")?);
            match &stack.inspect().kind {
                TokenKind::Identifier(s) => vars.push(VariableDeclName::new(s.clone(), stack.inspect().span())),
                _ => return Err(ParseError::UnexpectedTokenFound {
                    expect: "identifier",
                    span: stack.inspect().span(),
                })
            }

            stack.push(self.next_or_err(", or ;")?);
            match &stack.inspect().kind {
                TokenKind::Comma => (),
                TokenKind::Semicolon => break Ok(VariableDeclaration::new(stack.span(), var1, vars)),
                _ => return Err(ParseError::UnexpectedTokenFound {
                    expect: ", or ;",
                    span: stack.inspect().span(),
                })
            }
        }
    }

    fn parse_expr_stmt(&mut self) -> Result<ExpressionStatement, ParseError> {
        let expr = self.parse_expr()?;

        let stack = TokenStack::new(self.next_or_err(";")?);
        if !matches!(stack.inspect().kind, TokenKind::Semicolon) {
            return Err(ParseError::UnexpectedTokenFound {
                expect: ";",
                span: stack.inspect().span(),
            })
        }

        Ok(ExpressionStatement::new(expr.span() + stack.span(), expr))
    }

    fn parse_return(&mut self) -> Result<ReturnStatement, ParseError> {
        let mut stack = TokenStack::new(self.next_or_err("return")?);
        match &stack.inspect().kind {
            TokenKind::Return => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: "return",
                span: stack.inspect().span(),
            })
        }

        stack.push(self.peek_or_err("expression or ;")?);
        if matches!(stack.inspect().kind, TokenKind::Semicolon) {
            self.next();
            return Ok(ReturnStatement::new(stack.span(), None));
        }

        let expr = self.parse_expr()?;
        stack.push(self.next_or_err(";")?);
        match &stack.inspect().kind {
            TokenKind::Semicolon => {
                Ok(ReturnStatement::new(stack.span(), Some(expr)))
            }
            _ => Err(ParseError::UnexpectedTokenFound {
                expect: ";",
                span: stack.inspect().span(),
            })
        }
    }

    fn parse_break(&mut self) -> Result<BreakStatement, ParseError> {
        let mut stack = TokenStack::new(self.next_or_err("break")?);
        match &stack.inspect().kind {
            TokenKind::Break => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: "break",
                span: stack.inspect().span(),
            })
        }
        stack.push(self.next_or_err(";")?);
        match &stack.inspect().kind {
            TokenKind::Semicolon => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: ";",
                span: stack.inspect().span(),
            })
        }
        Ok(BreakStatement::new(stack.span()))
    }

    fn parse_continue(&mut self) -> Result<ContinueStatement, ParseError> {
        let mut stack = TokenStack::new(self.next_or_err("continue")?);
        match &stack.inspect().kind {
            TokenKind::Continue => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: "continue",
                span: stack.inspect().span(),
            })
        }
        stack.push(self.next_or_err(";")?);
        match &stack.inspect().kind {
            TokenKind::Semicolon => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: ";",
                span: stack.inspect().span(),
            })
        }
        Ok(ContinueStatement::new(stack.span()))
    }

    fn parse_for(&mut self) -> Result<ForStatement, ParseError> {
        let mut stack = TokenStack::new(self.next_or_err("for")?);
        if !matches!(stack.inspect().kind, TokenKind::For) {
            return Err(ParseError::UnexpectedTokenFound {
                expect: "for",
                span: stack.inspect().span()
            });
        }

        stack.push(self.next_or_err("(")?);
        if !matches!(stack.inspect().kind, TokenKind::LParen) {
            return Err(ParseError::UnexpectedTokenFound {
                expect: "(",
                span: stack.inspect().span()
            });
        }

        stack.push(self.peek_or_err("expression or ;")?);
        let init = match &stack.inspect().kind {
            TokenKind::Semicolon => {
                self.next();
                None
            }
            _ => {
                let expr = self.parse_expr()?;
                stack.push(self.next_or_err(";")?);
                match &stack.inspect().kind {
                    TokenKind::Semicolon => Some(expr),
                    _ => return Err(ParseError::UnexpectedTokenFound {
                        expect: ";",
                        span: stack.inspect().span(),
                    })
                }
            }
        };

        stack.push(self.peek_or_err("expression or ;")?);
        let cond = match &stack.inspect().kind {
            TokenKind::Semicolon => {
                self.next();
                None
            }
            _ => {
                let expr = self.parse_expr()?;
                stack.push(self.next_or_err(";")?);
                match &stack.inspect().kind {
                    TokenKind::Semicolon => Some(expr),
                    _ => return Err(ParseError::UnexpectedTokenFound {
                        expect: ";",
                        span: stack.inspect().span(),
                    })
                }
            }
        };

        stack.push(self.peek_or_err("expression or )")?);
        let update = match &stack.inspect().kind {
            TokenKind::RParen => {
                self.next();
                None
            }
            _ => {
                let expr = self.parse_expr()?;
                stack.push(self.next_or_err(")")?);
                match &stack.inspect().kind {
                    TokenKind::RParen => Some(expr),
                    _ => return Err(ParseError::UnexpectedTokenFound {
                        expect: ")",
                        span: stack.inspect().span(),
                    })
                }
            }
        };

        let body = self.parse_block()?;

        Ok(ForStatement::new(stack.span() + body.span(), init, cond, update, body))
    }

    fn parse_while(&mut self) -> Result<WhileStatement, ParseError> {
        let mut stack = TokenStack::new(self.next_or_err("while")?);
        if !matches!(stack.inspect().kind, TokenKind::While) {
            return Err(ParseError::UnexpectedTokenFound {
                expect: "while",
                span: stack.inspect().span(),
            })
        }

        stack.push(self.next_or_err("(")?);
        if !matches!(stack.inspect().kind, TokenKind::LParen) {
            return Err(ParseError::UnexpectedTokenFound {
                expect: "(",
                span: stack.inspect().span(),
            })
        }

        let cond = self.parse_expr()?;

        stack.push(self.next_or_err(")")?);
        if !matches!(stack.inspect().kind, TokenKind::RParen) {
            return Err(ParseError::UnexpectedTokenFound {
                expect: ")",
                span: stack.inspect().span(),
            })
        }

        let body = self.parse_block()?;

        Ok(WhileStatement::new(stack.span() + body.span(), cond, body))
    }

    fn parse_if(&mut self) -> Result<IfStatement, ParseError> {
        let mut stack = TokenStack::new(self.next_or_err("if")?);
        stack.push(self.next_or_err("(")?);
        match &stack.inspect().kind {
            TokenKind::LParen => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: "(",
                span: stack.inspect().span(),
            })
        }

        let cond = self.parse_expr()?;

        stack.push(self.next_or_err(")")?);
        match &stack.inspect().kind {
            TokenKind::RParen => (),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: ")",
                span: stack.inspect().span(),
            })
        }

        let body = self.parse_block()?;

        let Some(token) = self.next() else {
            return Ok(IfStatement::new(stack.span(), cond, body, None))
        };

        stack.push(token);
        match &stack.inspect().kind {
            TokenKind::Else => (),
            _ => return Ok(IfStatement::new(stack.span(), cond, body, None))
        }

        let else_body = self.parse_block()?;

        Ok(IfStatement::new(stack.span(), cond, body, Some(else_body)))
    }

    fn parse_block(&mut self) -> Result<BlockStatement, ParseError> {
        let mut stack = TokenStack::new(self.next_or_err("{")?);
        if !matches!(stack.inspect().kind, TokenKind::LCurly) {
            return Err(ParseError::UnexpectedTokenFound {
                expect: "{",
                span: stack.inspect().span(),
            })
        }

        let mut body = Vec::new();
        loop {
            stack.push(self.peek_or_err("statement or }")?);
            if matches!(stack.inspect().kind, TokenKind::RCurly) {
                self.next();
                break Ok(BlockStatement::new(stack.span(), body));
            } else {
                body.push(self.parse_stmt()?);
            }
        }
    }
}

impl NonEmptyParser {
    fn parse_expr(&mut self) -> Result<Expression, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_equality()?;
        while let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::Assign => {
                    self.next();
                    lhs = VariableAssignment::new(
                        Box::new(lhs), Box::new(self.parse_assignment()?)
                    ).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_equality(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_relative()?;
        while let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::EQ => {
                    self.next();
                    lhs = InfixExpression::new(
                        InfixOp::EQ, Box::new(lhs), Box::new(self.parse_relative()?)
                    ).into();
                }
                TokenKind::NE => {
                    self.next();
                    lhs = InfixExpression::new(
                        InfixOp::NE, Box::new(lhs), Box::new(self.parse_relative()?)
                    ).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_relative(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_additive()?;
        while let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::LT => {
                    self.next();
                    lhs = InfixExpression::new(
                        InfixOp::LT, Box::new(lhs), Box::new(self.parse_additive()?)
                    ).into();
                }
                TokenKind::GT => {
                    self.next();
                    lhs = InfixExpression::new(
                        InfixOp::GT, Box::new(lhs), Box::new(self.parse_additive()?)
                    ).into();
                }
                TokenKind::LE => {
                    self.next();
                    lhs = InfixExpression::new(
                        InfixOp::LE, Box::new(lhs), Box::new(self.parse_additive()?)
                    ).into();
                }
                TokenKind::GE => {
                    self.next();
                    lhs = InfixExpression::new(
                        InfixOp::GE, Box::new(lhs), Box::new(self.parse_additive()?)
                    ).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_additive(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_multiplicative()?;
        while let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::Plus => {
                    self.next();
                    lhs = InfixExpression::new(
                        InfixOp::Add, Box::new(lhs), Box::new(self.parse_multiplicative()?)
                    ).into();
                }
                TokenKind::Minus => {
                    self.next();
                    lhs = InfixExpression::new(
                        InfixOp::Sub, Box::new(lhs), Box::new(self.parse_multiplicative()?)
                    ).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_multiplicative(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_prefix()?;
        while let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::Star => {
                    self.next();
                    lhs = InfixExpression::new(
                        InfixOp::Mul, Box::new(lhs), Box::new(self.parse_prefix()?)
                    ).into();
                }
                TokenKind::Slash => {
                    self.next();
                    lhs = InfixExpression::new(
                        InfixOp::Div, Box::new(lhs), Box::new(self.parse_prefix()?)
                    ).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_prefix(&mut self) -> Result<Expression, ParseError> {
        let stack = TokenStack::new(self.peek_or_err("-")?);
        match &stack.inspect().kind {
            TokenKind::Minus => {
                self.next();
                let expr = self.parse_prefix()?;
                Ok(PrefixExpression::new(stack.span() + expr.span(), PrefixOp::Negation, Box::new(expr)).into())
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        let mut stack = TokenStack::new(self.next_or_err("integer, identifier or (")?);
        match &stack.inspect().kind {
            TokenKind::LParen => {
                let expr = self.parse_expr()?;
                stack.push(self.next_or_err(")")?);
                match &stack.inspect().kind {
                    TokenKind::RParen => Ok(expr),
                    _ => return Err(ParseError::UnexpectedTokenFound {
                        expect: ")",
                        span: stack.span()
                    })
                }
            }
            TokenKind::Integer(value) => Ok(IntegerExpression::new(stack.span(), *value).into()),
            TokenKind::Identifier(s) => Ok(VariableExpression::new(stack.span(), s.clone()).into()),
            _ => return Err(ParseError::UnexpectedTokenFound {
                expect: "integer, identifier or (",
                span: stack.inspect().span()
            })
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
