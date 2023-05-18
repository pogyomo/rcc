use std::str::CharIndices;

const EOF_CHAR: char = '\0';

pub struct LexInput<'a> {
    chars: CharIndices<'a>,
    len: usize,
}

impl<'a> LexInput<'a> {
    pub fn new(input: &'a str) -> LexInput<'a> {
        Self {
            chars: input.char_indices(),
            len: input.len(),
        }
    }

    pub fn is_eof(&self) -> bool {
        self.chars.clone().next().is_none()
    }

    pub fn offset(&self) -> usize {
        self.chars.clone().nth(0).map_or(self.len, |v| v.0)
    }

    pub fn first(&self) -> char {
        self.chars.clone().nth(0).map_or(EOF_CHAR, |v| v.1)
    }

    pub fn second(&self) -> char {
        self.chars.clone().nth(1).map_or(EOF_CHAR, |v| v.1)
    }

    pub fn next(&mut self) -> char {
        self.chars.next().map_or(EOF_CHAR, |v| v.1)
    }
}
