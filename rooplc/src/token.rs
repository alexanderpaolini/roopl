use std::fmt;

#[derive(Clone)]
pub struct Position {
    pub idx: i32,
    pub len: i32,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Minus,
    Plus,
    Star,
    Slash,
    Bang,
    BangEquals,
    Equals,
    EqualsEquals,
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
    EoF,
    Number,
    String,
    InvalidToken,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone)]
pub struct Token {
    pub pos: Position,
    pub kind: TokenKind,
    pub content: Option<String>,
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
            self.content.as_deref().unwrap_or(""),
        )
    }
}
