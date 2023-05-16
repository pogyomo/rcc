use std::ops::{Add, AddAssign};

/// A trait for the object which have its span.
pub trait Spannable {
    fn span(&self) -> CodeSpan;
}

/// A struct which indicate the range of source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct CodeSpan {
    offset: usize,
    len: usize,
}

impl CodeSpan {
    pub fn new(offset: usize, len: usize) -> Self {
        Self { offset, len }
    }
}

impl Add for CodeSpan {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        if self.offset <= rhs.offset {
            if self.offset + self.len < rhs.offset + rhs.len {
                Self {
                    offset: self.offset,
                    len: rhs.offset - self.offset + rhs.len,
                }
            } else {
                Self {
                    offset: self.offset,
                    len: self.len
                }
            }
        } else {
            rhs.add(self)
        }
    }
}

impl AddAssign for CodeSpan {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

#[cfg(test)]
mod test {
    use super::CodeSpan;

    #[test]
    fn wrapping() {
        let wrapper = CodeSpan::new(0, 10);
        let wrapped = CodeSpan::new(2, 5);
        assert_eq!(wrapper + wrapped, wrapper);
        assert_eq!(wrapped + wrapper, wrapper);
    }

    #[test]
    fn have_same_region() {
        let lhs = CodeSpan::new(0, 10);
        let rhs = CodeSpan::new(5, 10);
        let res = CodeSpan::new(0, 15);
        assert_eq!(lhs + rhs, res);
        assert_eq!(rhs + lhs, res);
    }

    #[test]
    fn dont_have_same_region() {
        let lhs = CodeSpan::new(0, 10);
        let rhs = CodeSpan::new(12, 3);
        let res = CodeSpan::new(0, 15);
        assert_eq!(lhs + rhs, res);
        assert_eq!(rhs + lhs, res);
    }
}
