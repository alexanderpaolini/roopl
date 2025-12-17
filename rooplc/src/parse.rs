use crate::{
    ast::*,
    token::{self, Token, TokenKind},
};

pub struct Parser {
    toks: Vec<Token>,
    idx: usize,
    out: Program,

    next_expr_id: ExprId,
    next_stmt_id: StmtId,
    next_class_id: ClassId,
}

impl Parser {
    pub fn new(toks: Vec<Token>) -> Parser {
        Self {
            toks,
            idx: 0,
            out: Program { items: vec![] },
            next_expr_id: 1,
            next_stmt_id: 1,
            next_class_id: 1,
        }
    }

    fn generate_expr_id(&mut self) -> ExprId {
        let id = self.next_expr_id;
        self.next_expr_id += 1;
        id
    }

    fn generate_stmt_id(&mut self) -> StmtId {
        let id = self.next_stmt_id;
        self.next_stmt_id += 1;
        id
    }

    fn generate_class_id(&mut self) -> StmtId {
        let id = self.next_stmt_id;
        self.next_class_id += 1;
        id
    }

    fn make_stmt(&mut self, kind: StmtKind) -> Stmt {
        Stmt {
            id: self.generate_stmt_id(),
            kind,
        }
    }

    fn make_expr(&mut self, kind: ExprKind) -> Expr {
        Expr {
            id: self.generate_expr_id(),
            kind,
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
                    found: tok,
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

    pub fn parse_return_statement(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::Return)?;

        let ret_val = if let Some(tok) = self.peek() {
            if tok.kind != TokenKind::SemiColon {
                Some(self.parse_expression()?)
            } else {
                None
            }
        } else {
            None
        };

        self.expect(TokenKind::SemiColon)?;

        Ok(self.make_stmt(StmtKind::Return(ReturnStmt { value: ret_val })))
    }

    pub fn parse_expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let res = self.parse_expression()?;

        Ok(self.make_stmt(StmtKind::Expr(res)))
    }

    pub fn parse_variable_declaration_statement(&mut self) -> Result<Stmt, ParseError> {
        let ty = self.parse_type()?;
        let name = self.expect(TokenKind::Identifier)?.content.unwrap();

        let mut initializer = None;
        if let Some(tok) = self.peek()
            && tok.kind == TokenKind::Equals
        {
            self.consume();
            initializer = Some(self.parse_expression()?);
        }

        Ok(self.make_stmt(StmtKind::VarDecl(VarDeclStmt {
            name,
            ty,
            initializer,
        })))
    }

    pub fn parse_assignment_statement(&mut self) -> Result<Stmt, ParseError> {
        let target = self.parse_expression()?;

        self.expect(TokenKind::Equals)?;

        let value = self.parse_expression()?;

        Ok(self.make_stmt(StmtKind::Assignment(AssignmentStmt { target, value })))
    }

    pub fn parse_if_statement(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::If)?;
        self.expect(TokenKind::LeftParen)?;

        let condition = self.parse_expression()?;

        self.expect(TokenKind::RightParen)?;

        let then_branch = Box::new(self.parse_statement()?);

        let else_branch = if let Some(tok) = self.peek() {
            if tok.kind == TokenKind::Else {
                self.consume();
                Some(Box::new(self.parse_statement()?))
            } else {
                None
            }
        } else {
            None
        };

        Ok(self.make_stmt(StmtKind::If(IfStmt {
            condition,
            then_branch,
            else_branch,
        })))
    }

    pub fn parse_for_statement(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::For)?;

        self.expect(TokenKind::LeftParen)?;

        let init = if let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Identifier => {
                    let ntok = self.peek_next().ok_or(ParseError::EndOfInput)?;
                    match ntok.kind {
                        TokenKind::Identifier => Some(self.parse_variable_declaration_statement()?),
                        TokenKind::Equals => Some(self.parse_assignment_statement()?),
                        _ => Some(self.parse_expression_statement()?),
                    }
                }
                _ => None,
            }
        } else {
            None
        };

        self.expect(TokenKind::SemiColon)?;

        let condition = self.parse_expression()?;

        self.expect(TokenKind::SemiColon)?;

        let update = if let Some(tok) = self.peek()
            && tok.kind == TokenKind::Identifier
            && let Some(next_tok) = self.peek_next()
        {
            if next_tok.kind == TokenKind::LeftParen {
                None
            } else if next_tok.kind == TokenKind::Equals {
                Some(self.parse_assignment_statement()?)
            } else {
                Some(self.parse_expression_statement()?)
            }
        } else {
            Some(self.parse_expression_statement()?)
        };

        self.expect(TokenKind::RightParen)?;

        let stmt = self.parse_statement()?;

        Ok(self.make_stmt(StmtKind::For(ForStmt {
            init: init.map(Box::new),
            condition,
            update: update.map(Box::new),
            body: Box::new(stmt),
        })))
    }

    pub fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        let ctok = self.peek().ok_or(ParseError::EndOfInput)?;

        match ctok.kind {
            TokenKind::Identifier => {
                let ntok = self.peek_next().ok_or(ParseError::EndOfInput)?;

                let stmt = match ntok.kind {
                    TokenKind::Identifier => self.parse_variable_declaration_statement()?,

                    _ => {
                        let target = self.parse_expression()?;

                        if let Some(nntok) = self.peek()
                            && nntok.kind == TokenKind::Equals
                        {
                            self.expect(TokenKind::Equals)?;

                            let value: Expr = self.parse_expression()?;

                            self.make_stmt(StmtKind::Assignment(AssignmentStmt { target, value }))
                        } else {
                            self.make_stmt(StmtKind::Expr(target))
                        }
                    }
                };

                self.expect(TokenKind::SemiColon)?;
                Ok(stmt)
            }

            TokenKind::For => self.parse_for_statement(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::LeftBracket => self.parse_block_statement(),
            TokenKind::Return => self.parse_return_statement(),

            _ => {
                let stmt = self.parse_expression_statement()?;
                self.expect(TokenKind::SemiColon)?;
                Ok(stmt)
            }
        }
    }

    pub fn parse_block_statement(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::LeftBracket)?;

        let mut stmts: Vec<Stmt> = vec![];

        while let Some(tok) = self.peek()
            && tok.kind != TokenKind::RightBracket
        {
            stmts.push(self.parse_statement()?);
        }

        self.expect(TokenKind::RightBracket)?;

        Ok(self.make_stmt(StmtKind::Block(BlockStmt { stmts })))
    }

    pub fn parse_type(&mut self) -> Result<Type, ParseError> {
        // TODO: a type can be more than just a name at this point
        // But idc so for now its just gonna be a name. Ideally we get templating at some point
        // or whatnot.
        let ty = self.expect(TokenKind::Identifier)?.content.unwrap();

        Ok(match ty.as_str() {
            "int" => Type::Int,
            "float" => Type::Float,
            "string" => Type::String,
            "void" => Type::Void,
            _ => Type::Named(ty),
        })
    }

    pub fn parse_paramater(&mut self) -> Result<Parameter, ParseError> {
        let name = self.expect(TokenKind::Identifier)?.content.unwrap();
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;

        Ok(Parameter { name, ty })
    }

    pub fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, ParseError> {
        let mut res: Vec<Parameter> = vec![];

        if let Some(ctok) = self.peek()
            && ctok.kind == TokenKind::Identifier
        {
            res.push(self.parse_paramater()?);
        }

        while let Some(ntok) = self.peek()
            && ntok.kind == TokenKind::Comma
        {
            self.expect(TokenKind::Comma)?;
            res.push(self.parse_paramater()?);
        }

        Ok(res)
    }

    pub fn parse_method(&mut self, is_static: bool) -> Result<ClassMember, ParseError> {
        let name = self.expect(TokenKind::Identifier)?.content.unwrap();
        self.expect(TokenKind::LeftParen)?;

        let params = self.parse_parameter_list()?;

        self.expect(TokenKind::RightParen)?;

        self.expect(TokenKind::Colon)?;

        let return_type = self.parse_type()?;

        let body = self.parse_block_statement()?;

        Ok(ClassMember::Method(MethodMember {
            name,
            params,
            return_type,
            body,
            is_static,
        }))
    }

    pub fn parse_property(&mut self, is_static: bool) -> Result<ClassMember, ParseError> {
        let name = self.expect(TokenKind::Identifier)?.content.unwrap();
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;

        let mut value = None;
        if let Some(tok) = self.peek()
            && tok.kind == TokenKind::Equals
        {
            self.expect(TokenKind::Equals)?;
            value = Some(self.parse_expression()?);
        }

        self.expect(TokenKind::SemiColon)?;

        Ok(ClassMember::Property(PropertyMember {
            name,
            ty,
            initializer: value,
            is_static,
        }))
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
                expr = self.make_expr(ExprKind::Binary(BinaryExpr {
                    left: Box::new(expr),
                    op: BinaryOp::LogicalOr,
                    right: Box::new(right),
                }));
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
                expr = self.make_expr(ExprKind::Binary(BinaryExpr {
                    left: Box::new(expr),
                    op: BinaryOp::LogicalAnd,
                    right: Box::new(right),
                }));
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
                TokenKind::EqualsEquals | TokenKind::BangEquals => {
                    let op = if tok.kind == TokenKind::EqualsEquals {
                        BinaryOp::Equals
                    } else {
                        BinaryOp::NotEquals
                    };
                    self.consume();
                    let right = self.parse_relational()?;
                    expr = self.make_expr(ExprKind::Binary(BinaryExpr {
                        left: Box::new(expr),
                        op,
                        right: Box::new(right),
                    }));
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
                    expr = self.make_expr(ExprKind::Binary(BinaryExpr {
                        left: Box::new(expr),
                        op,
                        right: Box::new(right),
                    }));
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
                    expr = self.make_expr(ExprKind::Binary(BinaryExpr {
                        left: Box::new(expr),
                        op,
                        right: Box::new(right),
                    }));
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
                    expr = self.make_expr(ExprKind::Binary(BinaryExpr {
                        left: Box::new(expr),
                        op,
                        right: Box::new(right),
                    }));
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
                    let expr = self.parse_postfix()?;
                    return Ok(self.make_expr(ExprKind::Unary(UnaryExpr {
                        op,
                        expr: Box::new(expr),
                    })));
                }
                _ => {}
            }
        }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.peek() {
                Some(tok) if tok.kind == TokenKind::LeftParen => {
                    self.expect(TokenKind::LeftParen)?;
                    let args = self.parse_argument_list()?;
                    self.expect(TokenKind::RightParen)?;

                    expr = self.make_expr(ExprKind::Call(CallExpr {
                        callee: Box::new(expr),
                        args,
                    }));
                }

                Some(tok) if tok.kind == TokenKind::RightArrow => {
                    self.consume();
                    let field = self.expect(TokenKind::Identifier)?;

                    expr = self.make_expr(ExprKind::Access(AccessExpr {
                        object: Box::new(expr),
                        field: field.content.unwrap(),
                    }));
                }

                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        if let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Number => {
                    let value = tok.content.unwrap().parse::<f64>().unwrap();
                    self.consume();
                    return Ok(self.make_expr(ExprKind::Literal(Literal::Number(value))));
                }
                TokenKind::String => {
                    let value = tok.content.unwrap();
                    self.consume();
                    return Ok(self.make_expr(ExprKind::Literal(Literal::String(value))));
                }
                TokenKind::Identifier => {
                    let name = tok.content.unwrap();
                    self.consume();
                    return Ok(self.make_expr(ExprKind::Variable(name)));
                }
                TokenKind::LeftParen => {
                    self.consume();
                    let expr = self.parse_expression()?;
                    self.expect(TokenKind::RightParen)?;
                    return Ok(self.make_expr(ExprKind::Grouping(GroupingExpr {
                        expr: Box::new(expr),
                    })));
                }
                _ => {}
            }
        }

        Err(ParseError::UnexpectedToken {
            expected: TokenKind::Identifier,
            found: self.peek().unwrap_or_else(|| Token {
                pos: token::Position { idx: 0, len: 0 },
                kind: TokenKind::EoF,
                content: Some("EOF".to_string()),
            }),
        })
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();

        if let Some(tok) = self.peek()
            && tok.kind == TokenKind::RightParen
        {
            return Ok(args);
        }

        loop {
            args.push(self.parse_expression()?);

            if let Some(ntok) = self.peek()
                && ntok.kind == TokenKind::Comma
            {
                self.expect(TokenKind::Comma)?;
                continue;
            }

            break;
        }

        Ok(args)
    }

    pub fn parse_class_member(&mut self) -> Result<ClassMember, ParseError> {
        let mut is_static = false;

        if let Some(stok) = self.peek()
            && stok.kind == TokenKind::Identifier
            && stok.content.unwrap() == "static"
        {
            self.expect(TokenKind::Identifier)?;
            is_static = true;
        }

        if let Some(ctok) = self.peek_next() {
            match ctok.kind {
                TokenKind::Colon => self.parse_property(is_static),
                TokenKind::LeftParen => self.parse_method(is_static),
                _ => Err(ParseError::NotImplemented),
            }
        } else {
            Err(ParseError::EndOfInput)
        }
    }

    pub fn parse_class_members(&mut self) -> Result<Vec<ClassMember>, ParseError> {
        let mut res: Vec<ClassMember> = vec![];

        while let Some(ctok) = self.peek()
            && ctok.kind != TokenKind::RightBracket
        {
            res.push(self.parse_class_member()?);
        }

        Ok(res)
    }

    pub fn parse_class(&mut self) -> Result<Item, ParseError> {
        self.expect(TokenKind::Class)?;

        let mut class_decl = ClassDecl {
            id: self.generate_class_id(),
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

        class_decl.members = self.parse_class_members()?;

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
                        found: tok,
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
    use super::parse;
    use crate::{ast::*, lex::lex};

    fn assert_class(ast: &Program, name: &str, member_count: usize) -> ClassDecl {
        assert_eq!(ast.items.len(), 1);
        if let Item::Class(c) = &ast.items[0] {
            assert_eq!(c.name, name);
            assert_eq!(c.members.len(), member_count);
            c.clone()
        } else {
            panic!("Expected a class named '{}'", name);
        }
    }

    fn assert_property(member: &ClassMember, name: &str, ty: Type, is_static: bool) {
        if let ClassMember::Property(PropertyMember {
            name: n,
            ty: t,
            is_static: s,
            ..
        }) = member
        {
            assert_eq!(n, name);
            assert!(matches!(t, _ if *t == ty));
            assert_eq!(*s, is_static);
        } else {
            panic!("Expected a property named '{}'", name);
        }
    }

    fn assert_method(
        member: &ClassMember,
        name: &str,
        param_count: usize,
        return_ty: Type,
        is_static: bool,
    ) {
        if let ClassMember::Method(MethodMember {
            name: n,
            params,
            return_type,
            is_static: s,
            ..
        }) = member
        {
            assert_eq!(n, name);
            assert_eq!(params.len(), param_count);
            assert!(matches!(return_type, _ if *return_type == return_ty));
            assert_eq!(*s, is_static);
        } else {
            panic!("Expected a method named '{}'", name);
        }
    }

    #[test]
    fn test_with_props() {
        let toks = lex("
            class Main {
                static x : int = 0;
                y : float = 67.0;
            }
        "
        .to_string());

        let ast = parse(toks);
        let class = assert_class(&ast, "Main", 2);

        assert_property(&class.members[0], "x", Type::Int, true);
        assert_property(&class.members[1], "y", Type::Float, false);
    }

    #[test]
    fn test_with_methods() {
        let toks = lex("
            class Main {
                static main (argc: int, args: Array) : int {}
                test () : float {}
            }
        "
        .to_string());

        let ast = parse(toks);
        let class = assert_class(&ast, "Main", 2);

        assert_method(&class.members[0], "main", 2, Type::Int, true);
        assert_method(&class.members[1], "test", 0, Type::Float, false);
    }

    #[test]
    fn test_with_if() {
        let toks = lex("
            class Main {
                static main (argc: int, args: Array) : int {
                    if (argc > 0) { return 1; } else { return 0; }
                }
            }
        "
        .to_string());

        let ast = parse(toks);
        assert_class(&ast, "Main", 1);
    }

    #[test]
    fn test_with_for() {
        let toks = lex("
            class Main {
                static main (argc: int, args: Array) : int {
                    for (int i = 0; i < argc; i = i + 1) {
                        if (i == 5) { return i; }
                    }
                    return 0;
                }
            }
        "
        .to_string());

        let ast = parse(toks);
        assert_class(&ast, "Main", 1);
    }

    #[test]
    fn test_with_object_access() {
        let toks = lex("
            class Main {
                static main (argc: int, args: Array) : int {
                    obj->method();
                    obj->property = 42;
                }
            }
        "
        .to_string());

        let ast = parse(toks);
        let class = assert_class(&ast, "Main", 1);

        let method = &class.members[0];
        if let ClassMember::Method(MethodMember { body, .. }) = method {
            let stmts = match &body.kind {
                StmtKind::Block(BlockStmt { stmts }) => stmts,
                _ => panic!("Expected method body to be a block"),
            };
            assert_eq!(stmts.len(), 2);

            // First statement: method call
            if let StmtKind::Expr(expr) = &stmts[0].kind {
                if let ExprKind::Call(CallExpr { callee, .. }) = &expr.kind {
                    if let ExprKind::Access(AccessExpr { object, field }) = &callee.kind {
                        if let ExprKind::Variable(name) = &object.kind {
                            assert_eq!(name, "obj");
                            assert_eq!(field, "method");
                        } else {
                            panic!("Expected object to be variable 'obj'");
                        }
                    } else {
                        panic!("Expected a method call on 'obj'");
                    }
                } else {
                    panic!("Expected first statement to be a call");
                }
            } else {
                panic!("Expected first statement to be an expression");
            }

            // Second statement: assignment
            if let StmtKind::Assignment(AssignmentStmt { target, value }) = &stmts[1].kind {
                if let ExprKind::Access(AccessExpr { object, field }) = &target.kind {
                    if let ExprKind::Variable(name) = &object.kind {
                        assert_eq!(name, "obj");
                        assert_eq!(field, "property");
                    } else {
                        panic!("Expected object to be variable 'obj'");
                    }
                } else {
                    panic!("Expected assignment target to be access");
                }

                if let ExprKind::Literal(Literal::Number(num)) = &value.kind {
                    assert_eq!(*num, 42.0);
                } else {
                    panic!("Expected value to be number 42");
                }
            } else {
                panic!("Expected second statement to be assignment");
            }
        } else {
            panic!("Expected a method");
        }
    }
}
