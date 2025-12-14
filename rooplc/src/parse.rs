use crate::{
    ast::*,
    token::{Token, TokenKind},
};

pub struct Parser {
    toks: Vec<Token>,
    idx: usize,
    out: Program,
}

impl Parser {
    pub fn new(toks: Vec<Token>) -> Parser {
        Self {
            toks,
            idx: 0 as usize,
            out: Program { items: vec![] },
        }
    }

    pub fn peek(&self) -> Option<Token> {
        self.toks.get(self.idx).cloned()
    }

    pub fn peek_next(&self) -> Option<Token> {
        self.toks.get(self.idx).cloned()
    }

    pub fn consume(&mut self) -> Option<Token> {
        let cur = self.peek();
        self.idx = self.idx + 1;
        cur
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        if let Some(tok) = self.peek() {
            if tok.kind == kind {
                self.idx += 1;
                Ok(tok)
            } else {
                Err(ParseError::UnexpectedToken {
                    expected: kind,
                    found: tok.kind,
                })
            }
        } else {
            Err(ParseError::EndOfInput)
        }
    }

    pub fn parse_import(&mut self) -> Result<Item, ParseError> {
        self.expect(TokenKind::Import)?;

        let mut path = Vec::new();

        let first = self.expect(TokenKind::Identifier)?.content.unwrap();
        path.push(first);

        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Dot => {
                    self.expect(TokenKind::Dot)?;
                    let nid = self.expect(TokenKind::Identifier)?.content.unwrap();
                    path.push(nid);
                }
                _ => break,
            }
        }

        self.expect(TokenKind::SemiColon)?;

        Ok(Item::Import(ImportStmt { path }))
    }

    pub fn parse_class(&mut self) -> Result<Item, ParseError> {
        self.expect(TokenKind::Class)?;

        let mut class_decl = ClassDecl {
            name: self.expect(TokenKind::Identifier)?.content.unwrap(),
            base: None,
            members: Vec::new(),
        };

        if let Some(etok) = self.peek() {
            if etok.kind == TokenKind::LessThan {
                self.expect(TokenKind::LessThan)?;
                class_decl.base =
                    Some(self.expect(TokenKind::Identifier).unwrap().content.unwrap());
            }
        }

        self.expect(TokenKind::LeftBracket)?;

        self.expect(TokenKind::RightBracket)?;

        Ok(Item::Class(class_decl))
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        while let Some(tok) = self.peek() {
            let item = match tok.kind {
                TokenKind::Import => self.parse_import()?,
                TokenKind::Class => self.parse_class()?,
                TokenKind::EoF => break,
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: TokenKind::EoF,
                        found: tok.kind,
                    });
                }
            };
            self.out.items.push(item);
        }

        Ok(self.out.clone())
    }
}

pub fn parse(toks: Vec<Token>) -> Program {
    Parser::new(toks)
        .parse()
        .unwrap_or_else(|err| panic!("ParseError: {:?}", err))
}

#[cfg(test)]
mod tests {
    use crate::{ast::*, lex::lex};

    use super::parse;

    #[test]
    fn test_empty_input() {
        let ast = parse(vec![]);

        println!("{:?}", ast)
    }

    #[test]
    fn test_main_class() {
        let toks = lex("class Main {}".to_string());
        let ast = parse(toks);

        assert_eq!(ast.items.len(), 1);
        assert!(
            matches!(ast.items[0], Item::Class(ClassDecl { ref name, ref members, .. }) if name == "Main" && members.is_empty())
        );
    }

    #[test]
    fn test_import_stmt() {
        let toks = lex("import a.b.c;".to_string());
        let ast = parse(toks);

        assert_eq!(ast.items.len(), 1);
        println!("{:?}", ast)
    }

    #[test]
    #[should_panic(expected = "ParseError")]
    fn test_import_err() {
        let toks = lex("import a.b.;".to_string());
        parse(toks);
    }
}
