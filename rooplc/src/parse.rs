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
            idx: 0,
            out: Program { items: vec![] },
        }
    }

    pub fn peek(&self) -> Option<Token> {
        self.toks.get(self.idx).cloned()
    }

    pub fn peek_next(&self) -> Option<Token> {
        self.toks.get(self.idx + 1).cloned()
    }

    pub fn consume(&mut self) -> Option<Token> {
        let cur = self.peek();
        self.idx += 1;
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

    pub fn parse_method(&mut self) -> Result<ClassMember, ParseError> {
        Err(ParseError::NotImplemented)
    }

    pub fn parse_property(&mut self) -> Result<ClassMember, ParseError> {
        let name = self.expect(TokenKind::Identifier)?.content.unwrap();
        self.expect(TokenKind::Colon)?;
        let ty = self.expect(TokenKind::Identifier)?.content.unwrap();

        let mut value = None;
        if let Some(tok) = self.peek()
            && tok.kind == TokenKind::Equals
        {
            self.expect(TokenKind::Equals)?;
            value = Some(self.parse_expression()?);
        }

        self.expect(TokenKind::SemiColon)?;

        Ok(ClassMember::Property {
            name,
            ty: match ty.as_str() {
                "int" => Type::Int,
                "float" => Type::Float,
                "string" => Type::String,
                _ => Type::Named(ty),
            },
            initializer: value,
        })
    }

    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_logical_or()
    }

    fn parse_logical_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_logical_and()?;

        while let Some(tok) = self.peek() {
            if tok.kind == TokenKind::DoublePipe {
                self.consume();
                let right = self.parse_logical_and()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: BinaryOp::LogicalOr,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_equality()?;

        while let Some(tok) = self.peek() {
            if tok.kind == TokenKind::And {
                self.consume();
                let right = self.parse_equality()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: BinaryOp::LogicalAnd,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_relational()?;

        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Equals => {
                    let op = if tok.kind == TokenKind::EqualsEquals {
                        BinaryOp::Equals
                    } else {
                        BinaryOp::NotEquals
                    };
                    self.consume();
                    let right = self.parse_relational()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_relational(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_additive()?;

        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::LessThan
                | TokenKind::GreaterThan
                | TokenKind::LessThanEqualTo
                | TokenKind::GreaterThanEqualTo => {
                    let op = match tok.kind {
                        TokenKind::LessThan => BinaryOp::LessThan,
                        TokenKind::GreaterThan => BinaryOp::GreaterThan,
                        TokenKind::LessThanEqualTo => BinaryOp::LessThanEqualTo,
                        TokenKind::GreaterThanEqualTo => BinaryOp::GreaterThanEqualTo,
                        _ => unreachable!(),
                    };
                    self.consume();
                    let right = self.parse_additive()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_additive(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_multiplicative()?;

        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Plus | TokenKind::Minus => {
                    let op = if tok.kind == TokenKind::Plus {
                        BinaryOp::Plus
                    } else {
                        BinaryOp::Minus
                    };
                    self.consume();
                    let right = self.parse_multiplicative()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;

        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Star | TokenKind::Slash | TokenKind::Percent => {
                    let op = match tok.kind {
                        TokenKind::Star => BinaryOp::Times,
                        TokenKind::Slash => BinaryOp::Divide,
                        TokenKind::Percent => BinaryOp::Mod,
                        _ => unreachable!(),
                    };
                    self.consume();
                    let right = self.parse_unary()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Minus | TokenKind::Bang => {
                    let op = if tok.kind == TokenKind::Minus {
                        UnaryOp::Negate
                    } else {
                        UnaryOp::Not
                    };
                    self.consume();
                    let expr = self.parse_primary()?;
                    return Ok(Expr::Unary {
                        op,
                        expr: Box::new(expr),
                    });
                }
                _ => {}
            }
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        if let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Number => {
                    let value = tok.content.unwrap().parse::<f64>().unwrap();
                    self.consume();
                    return Ok(Expr::Literal(Literal::Number(value)));
                }
                TokenKind::String => {
                    let value = tok.content.unwrap();
                    self.consume();
                    return Ok(Expr::Literal(Literal::String(value)));
                }
                TokenKind::Identifier => {
                    let name = tok.content.unwrap();
                    self.consume();
                    return Ok(Expr::Variable(name));
                }
                TokenKind::LeftParen => {
                    self.consume();
                    let expr = self.parse_expression()?;
                    self.expect(TokenKind::RightParen)?;
                    return Ok(expr);
                }
                _ => {}
            }
        }

        Err(ParseError::UnexpectedToken {
            expected: TokenKind::Identifier,
            found: self.peek().map_or(TokenKind::EoF, |t| t.kind),
        })
    }

    pub fn parse_class(&mut self) -> Result<Item, ParseError> {
        self.expect(TokenKind::Class)?;

        let mut class_decl = ClassDecl {
            name: self.expect(TokenKind::Identifier)?.content.unwrap(),
            base: None,
            members: Vec::new(),
        };

        if let Some(etok) = self.peek()
            && etok.kind == TokenKind::LessThan
        {
            self.expect(TokenKind::LessThan)?;
            class_decl.base = Some(self.expect(TokenKind::Identifier)?.content.unwrap());
        }

        self.expect(TokenKind::LeftBracket)?;

        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Identifier => {
                    if let Some(next_tok) = self.peek_next() {
                        match next_tok.kind {
                            TokenKind::Identifier => {
                                class_decl.members.push(self.parse_method()?);
                            }
                            TokenKind::Colon => {
                                class_decl.members.push(self.parse_property()?);
                            }
                            _ => break,
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }

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

    #[test]
    fn test_given_prop() {
        let toks = lex("
        import std.Math;
        class Main {
          x : int = 0;
          int main() {}
        }
        "
        .to_string());

        for tok in &toks {
            println!("{tok}");
        }
        let ast = parse(toks);
        println!("{ast}")
    }
}
