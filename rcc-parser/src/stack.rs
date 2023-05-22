use std::rc::Rc;
use nonempty::NonEmpty;
use rcc_codespan::{Spannable, CodeSpan};
use rcc_token::Token;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TokenStack {
    tokens: NonEmpty<Rc<Token>>
}

impl TokenStack {
    pub fn new(token: Rc<Token>) -> Self {
        Self { tokens: NonEmpty::new(token) }
    }

    pub fn push(&mut self, token: Rc<Token>) {
        self.tokens.push(token);
    }

    pub fn pop(&mut self) -> Option<Rc<Token>> {
        self.tokens.pop()
    }

    pub fn inspect(&self) -> Rc<Token> {
        Rc::clone(self.tokens.last())
    }
}

impl Spannable for TokenStack {
    fn span(&self) -> CodeSpan {
        let mut span = self.tokens.first().span();
        for token in self.tokens.iter() {
            span += token.span();
        }
        span
    }
}
