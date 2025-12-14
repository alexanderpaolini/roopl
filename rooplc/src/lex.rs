use crate::token::{Position, Token, TokenKind};

pub struct Lexer {
    input: Vec<char>,
    idx: usize,
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Self {
            input: input.chars().collect(),
            idx: 0,
            tokens: Vec::new(),
        }
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.idx).copied()
    }

    fn peek_next(&self) -> Option<char> {
        self.input.get(self.idx + 1).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let current = self.peek();
        self.idx += 1;
        current
    }

    fn advance_n(&mut self, n: usize) {
        self.idx += n;
    }

    fn emit(&mut self, kind: TokenKind, start: i32, len: i32) {
        self.emit_content(kind, start, len, None);
    }

    fn emit_content(&mut self, kind: TokenKind, start: i32, len: i32, content: Option<String>) {
        self.tokens.push(Token {
            kind,
            pos: Position { idx: start, len },
            content,
        })
    }

    pub fn lex(mut self) -> Vec<Token> {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
                continue;
            }

            let start = self.idx as i32;
            if c.is_ascii_digit() {
                let mut seen_dot = true;
                while let Some(next) = self.peek() {
                    if next.is_ascii_digit() {
                        self.advance();
                        continue;
                    }

                    if next == '.' && seen_dot {
                        seen_dot = false;
                        self.advance();
                        continue;
                    }

                    break;
                }

                let content: String = self.input[start as usize..self.idx].iter().collect();

                self.emit_content(
                    TokenKind::Number,
                    start,
                    self.idx as i32 - start,
                    Some(content),
                );

                continue;
            }

            if c.is_ascii_alphabetic() || c == '_' {
                while let Some(next) = self.peek() {
                    if next.is_ascii_alphanumeric() || next == '_' {
                        self.advance();
                    } else {
                        break;
                    }
                }
                let text: String = self.input[start as usize..self.idx].iter().collect();
                let kind = match text.as_str() {
                    "class" => TokenKind::Class,
                    "return" => TokenKind::Return,
                    "import" => TokenKind::Import,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "while" => TokenKind::While,
                    "for" => TokenKind::For,
                    _ => TokenKind::Identifier,
                };

                if kind == TokenKind::Identifier {
                    let content: String = self.input[start as usize..self.idx].iter().collect();
                    self.emit_content(kind, start, self.idx as i32 - start, Some(content));
                } else {
                    self.emit(kind, start, self.idx as i32 - start);
                }
                continue;
            }

            match c {
                '+' => self.emit(TokenKind::Plus, start, 1),
                '-' => {
                    if self.peek_next() == Some('>') {
                        self.advance_n(2);
                        self.emit(TokenKind::RightArrow, start, 2);
                    } else {
                        self.emit(TokenKind::Minus, start, 1);
                    }
                }
                '*' => self.emit(TokenKind::Star, start, 1),
                '/' => {
                    if self.peek_next() == Some('/') {
                        self.advance_n(2);
                        while let Some(next) = self.peek() {
                            if next == '\n' {
                                break;
                            }
                            self.advance();
                        }
                        self.emit(TokenKind::Comment, start, self.idx as i32 - start);
                    } else {
                        self.emit(TokenKind::Slash, start, 1);
                    }
                }
                '"' => {
                    self.advance();
                    while let Some(next) = self.peek() {
                        if next == '"' {
                            break;
                        }
                        self.advance();
                    }
                    let content: String =
                        self.input[(start + 1) as usize..self.idx].iter().collect();
                    self.emit_content(
                        TokenKind::String,
                        start,
                        self.idx as i32 - start - 1,
                        Some(content),
                    );
                }
                ':' => self.emit(TokenKind::Colon, start, 1),
                ';' => self.emit(TokenKind::SemiColon, start, 1),
                '.' => self.emit(TokenKind::Dot, start, 1),
                '%' => self.emit(TokenKind::Percent, start, 1),
                '^' => self.emit(TokenKind::Caret, start, 1),
                '&' => {
                    if self.peek_next() == Some('&') {
                        self.advance_n(2);
                        self.emit(TokenKind::DoubleAnd, start, 2);
                    } else {
                        self.emit(TokenKind::And, start, 1);
                    }
                }
                '|' => {
                    if self.peek_next() == Some('|') {
                        self.advance_n(2);
                        self.emit(TokenKind::DoublePipe, start, 2);
                    } else {
                        self.emit(TokenKind::Pipe, start, 1);
                    }
                }
                '(' => self.emit(TokenKind::LeftParen, start, 1),
                ')' => self.emit(TokenKind::RightParen, start, 1),
                '{' => self.emit(TokenKind::LeftBracket, start, 1),
                '}' => self.emit(TokenKind::RightBracket, start, 1),
                '<' => {
                    if self.peek_next() == Some('=') {
                        self.advance_n(2);
                        self.emit(TokenKind::LessThanEqualTo, start, 2);
                    } else {
                        self.emit(TokenKind::LessThan, start, 1);
                    }
                }
                '>' => {
                    if self.peek_next() == Some('=') {
                        self.advance_n(2);
                        self.emit(TokenKind::GreaterThanEqualTo, start, 2);
                    } else {
                        self.emit(TokenKind::GreaterThan, start, 1);
                    }
                }
                '=' => self.emit(TokenKind::Equals, start, 1),
                _ => self.emit(TokenKind::InvalidToken, start, 1),
            }
            self.advance();
        }

        self.emit(TokenKind::EoF, self.idx as i32, 0);
        self.tokens
    }
}

pub fn lex(input: String) -> Vec<Token> {
    Lexer::new(input).lex()
}

#[cfg(test)]
mod tests {
    use super::{TokenKind, lex};

    #[test]
    fn test_empty_input() {
        let toks = lex("".to_string());
        assert_eq!(toks[0].kind, TokenKind::EoF);
    }

    #[test]
    fn test_string() {
        let toks = lex("\"str\"".to_string());
        assert_eq!(toks[0].kind, TokenKind::String);
        assert_eq!(toks[0].content, Some("str".to_string()));
        assert_eq!(toks[0].pos.idx, 0);
        assert_eq!(toks[0].pos.len, 3);
    }

    #[test]
    fn test_string_without_end() {
        let toks = lex("\"str".to_string());
        assert_eq!(toks[0].kind, TokenKind::String);
        assert_eq!(toks[0].content, Some("str".to_string()));
        assert_eq!(toks[0].pos.idx, 0);
        assert_eq!(toks[0].pos.len, 3);
    }

    #[test]
    fn test_string_without_end_and_more() {
        let toks = lex("String s = \"str".to_string());
        assert_eq!(toks[3].kind, TokenKind::String);
        assert_eq!(toks[3].content, Some("str".to_string()));
        assert_eq!(toks[3].pos.idx, 11);
        assert_eq!(toks[3].pos.len, 3);
    }

    #[test]
    fn test_main_class() {
        let toks = lex("class Main {}".to_string());

        assert_eq!(toks[0].kind, TokenKind::Class);
        assert_eq!(toks[0].pos.idx, 0);
        assert_eq!(toks[0].pos.len, 5);

        assert_eq!(toks[1].kind, TokenKind::Identifier);
        assert_eq!(toks[1].pos.idx, 6);
        assert_eq!(toks[1].pos.len, 4);
        assert_eq!(toks[1].content, Some("Main".to_string()));

        assert_eq!(toks[2].kind, TokenKind::LeftBracket);
        assert_eq!(toks[2].pos.idx, 11);
        assert_eq!(toks[2].pos.len, 1);

        assert_eq!(toks[3].kind, TokenKind::RightBracket);
        assert_eq!(toks[3].pos.idx, 12);
        assert_eq!(toks[3].pos.len, 1);
    }

    #[test]
    fn test_comment() {
        let toks = lex("// this is a comment".to_string());
        assert_eq!(toks[0].kind, TokenKind::Comment);
    }

    #[test]
    fn test_multiline() {
        let toks = lex("
    import std.IO;

    class Main
    {
        main(): int
        {
            IO.print(\"Hello, World!\\n\");

            return 0.1;
        }
    }"
        .to_string());

        assert_eq!(toks.len(), 27);
    }
}
