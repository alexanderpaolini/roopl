use std::fmt;

pub struct Position {
    pub idx: i32,
    pub len: i32,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Minus,
    Plus,
    Star,
    Slash,
    Equals,
    Comment,
    Percent,
    SemiColon,
    Colon,
    Caret,
    And,
    DoubleAnd,
    Pipe,
    DoublePipe,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Dot,
    RightArrow,
    LessThan,
    GreaterThan,
    LessThanEqualTo,
    GreaterThanEqualTo,
    Return,
    Class,
    Import,
    If,
    Else,
    While,
    For,
    Identifier,
    EOF,
    Number,
    String,
    InvalidToken,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub struct Token {
    pub pos: Position,
    pub kind: TokenKind,
    pub content: String,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}, {} ({}), {} {}",
            self.pos.idx,
            self.pos.idx + self.pos.len,
            self.pos.len,
            self.kind,
            self.content
        )
    }
}
