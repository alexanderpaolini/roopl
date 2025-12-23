use crate::{
    ast::*,
    token::{self, Token, TokenKind},
};

pub struct Parser {
    toks: Vec<Token>,
    idx: usize,
    pub out: Program,

    next_expr_id: ExprId,
    next_stmt_id: StmtId,
    next_class_id: ClassId,

    pub errors: Vec<ParseError>,
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
            errors: vec![],
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

    fn make_error_stmt(&mut self) -> Stmt {
        Stmt {
            id: self.generate_stmt_id(),
            kind: StmtKind::Error,
        }
    }

    fn make_expr(&mut self, kind: ExprKind) -> Expr {
        Expr {
            id: self.generate_expr_id(),
            kind,
        }
    }

    fn make_error_expr(&mut self) -> Expr {
        self.make_expr(ExprKind::Error)
    }

    pub fn peek(&self) -> Option<Token> {
        self.toks.get(self.idx).cloned()
    }

    pub fn peek_next(&self) -> Option<Token> {
        self.toks.get(self.idx + 1).cloned()
    }

    fn peek_kind(&self) -> Option<TokenKind> {
        self.peek().map(|t| t.kind)
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
                    expected: Expected::Token(kind),
                    found: tok,
                })
            }
        } else {
            Err(ParseError::EndOfInput)
        }
    }

    fn synchronize_expr(&mut self) {
        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::SemiColon
                | TokenKind::Comma
                | TokenKind::RightParen
                | TokenKind::RightBracket => return,
                _ => {
                    self.consume();
                }
            }
        }
    }

    fn synchronize_stmt(&mut self) {
        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::SemiColon => {
                    self.consume();
                    return;
                }
                TokenKind::Return | TokenKind::If | TokenKind::While | TokenKind::For => return,
                _ => {
                    self.consume();
                }
            }
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

    pub fn parse_return_statement(&mut self) -> Stmt {
        if let Err(err) = self.expect(TokenKind::Return) {
            self.errors.push(err);
            self.synchronize_stmt();
            return self.make_error_stmt();
        }

        let ret_val = if let Some(tok) = self.peek() {
            if tok.kind != TokenKind::SemiColon {
                Some(self.parse_expression())
            } else {
                None
            }
        } else {
            None
        };

        if let Err(err) = self.expect(TokenKind::SemiColon) {
            self.errors.push(err);
            self.synchronize_stmt();
            return self.make_error_stmt();
        }

        self.make_stmt(StmtKind::Return(ReturnStmt { value: ret_val }))
    }

    pub fn parse_expression_statement(&mut self) -> Stmt {
        let res = self.parse_expression();

        self.make_stmt(StmtKind::Expr(res))
    }

    pub fn parse_variable_declaration_statement(&mut self) -> Stmt {
        let ty = match self.parse_type() {
            Ok(ty) => ty,
            Err(err) => {
                self.errors.push(err);
                self.synchronize_stmt();
                return self.make_error_stmt();
            }
        };

        let name = self.expect(TokenKind::Identifier);
        if let Err(err) = name {
            self.errors.push(err);
            self.synchronize_stmt();
            return self.make_error_stmt();
        }
        let name = name.unwrap().content.clone().unwrap();

        let mut initializer = None;
        if let Some(tok) = self.peek()
            && tok.kind == TokenKind::Equals
        {
            self.consume();
            initializer = Some(self.parse_expression());
        }

        self.make_stmt(StmtKind::VarDecl(VarDeclStmt {
            name,
            ty,
            initializer,
        }))
    }

    pub fn parse_assignment_statement(&mut self) -> Stmt {
        let target = self.parse_expression();

        self.consume();

        let value = self.parse_expression();

        self.make_stmt(StmtKind::Assignment(AssignmentStmt { target, value }))
    }

    pub fn parse_if_statement(&mut self) -> Stmt {
        if let Err(err) = self.expect(TokenKind::If) {
            self.errors.push(err);
            self.synchronize_stmt();
            return self.make_error_stmt();
        }

        if let Err(err) = self.expect(TokenKind::LeftParen) {
            self.errors.push(err);
            self.synchronize_stmt();
            return self.make_error_stmt();
        }

        let condition = self.parse_expression();

        if let Err(err) = self.expect(TokenKind::RightParen) {
            self.errors.push(err);
            self.synchronize_stmt();
            return self.make_error_stmt();
        }

        let then_branch = Box::new(self.parse_statement());

        let else_branch = if let Some(tok) = self.peek() {
            if tok.kind == TokenKind::Else {
                self.consume();
                Some(Box::new(self.parse_statement()))
            } else {
                None
            }
        } else {
            None
        };

        self.make_stmt(StmtKind::If(IfStmt {
            condition,
            then_branch,
            else_branch,
        }))
    }

    pub fn parse_for_statement(&mut self) -> Stmt {
        if let Err(err) = self.expect(TokenKind::For) {
            self.errors.push(err);
            self.synchronize_stmt();
            return self.make_error_stmt();
        }

        if let Err(err) = self.expect(TokenKind::LeftParen) {
            self.errors.push(err);
            self.synchronize_stmt();
            return self.make_error_stmt();
        }

        let init = if let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Identifier => {
                    let ntok = self.peek_next().ok_or(ParseError::EndOfInput);

                    if ntok.is_err() {
                        return self.make_error_stmt();
                    }

                    match ntok.unwrap().kind {
                        TokenKind::Identifier => Some(self.parse_variable_declaration_statement()),
                        TokenKind::Equals => Some(self.parse_assignment_statement()),
                        _ => Some(self.parse_expression_statement()),
                    }
                }
                _ => None,
            }
        } else {
            None
        };

        if let Err(err) = self.expect(TokenKind::SemiColon) {
            self.errors.push(err);
            self.synchronize_stmt();
            return self.make_error_stmt();
        }

        let condition = self.parse_expression();

        if let Err(err) = self.expect(TokenKind::SemiColon) {
            self.errors.push(err);
            self.synchronize_stmt();
            return self.make_error_stmt();
        }

        let update = if let Some(tok) = self.peek()
            && tok.kind == TokenKind::Identifier
            && let Some(next_tok) = self.peek_next()
        {
            if next_tok.kind == TokenKind::LeftParen {
                None
            } else if next_tok.kind == TokenKind::Equals {
                Some(self.parse_assignment_statement())
            } else {
                Some(self.parse_expression_statement())
            }
        } else {
            Some(self.parse_expression_statement())
        };

        if let Err(err) = self.expect(TokenKind::RightParen) {
            self.errors.push(err);
            self.synchronize_stmt();
            return self.make_error_stmt();
        }

        let stmt = self.parse_statement();

        self.make_stmt(StmtKind::For(ForStmt {
            init: init.map(Box::new),
            condition,
            update: update.map(Box::new),
            body: Box::new(stmt),
        }))
    }

    pub fn parse_statement(&mut self) -> Stmt {
        let ctok = self.peek();

        if ctok.is_none() {
            self.errors.push(ParseError::EndOfInput);
            return self.make_error_stmt();
        }
        let ctok = ctok.unwrap();

        match ctok.kind {
            TokenKind::For => self.parse_for_statement(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::LeftBracket => self.parse_block_statement(),
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::Identifier => {
                let ntok = self.peek_next().ok_or(ParseError::EndOfInput);

                if let Err(err) = ntok {
                    self.errors.push(err);
                    return self.make_error_stmt();
                }

                let ntok = ntok.unwrap();

                let stmt = match ntok.kind {
                    TokenKind::Identifier => self.parse_variable_declaration_statement(),
                    _ => {
                        let target = self.parse_expression();

                        if let Some(nntok) = self.peek()
                            && nntok.kind == TokenKind::Equals
                        {
                            self.consume();

                            let value: Expr = self.parse_expression();

                            self.make_stmt(StmtKind::Assignment(AssignmentStmt { target, value }))
                        } else {
                            self.make_stmt(StmtKind::Expr(target))
                        }
                    }
                };

                if let Err(err) = self.expect(TokenKind::SemiColon) {
                    self.errors.push(err);
                    self.synchronize_stmt();
                    return self.make_error_stmt();
                }

                stmt
            }
            _ => {
                let stmt = self.parse_expression_statement();
                if let Err(err) = self.expect(TokenKind::SemiColon) {
                    self.errors.push(err);
                    self.synchronize_stmt();
                    return self.make_error_stmt();
                }
                stmt
            }
        }
    }

    pub fn parse_block_statement(&mut self) -> Stmt {
        if let Err(err) = self.expect(TokenKind::LeftBracket) {
            self.errors.push(err);
            self.synchronize_stmt();
            return self.make_error_stmt();
        }

        let mut stmts: Vec<Stmt> = vec![];

        while let Some(tok) = self.peek()
            && tok.kind != TokenKind::RightBracket
        {
            stmts.push(self.parse_statement());
        }

        if let Err(err) = self.expect(TokenKind::RightBracket) {
            self.errors.push(err);
            self.synchronize_stmt();
            return self.make_error_stmt();
        }

        self.make_stmt(StmtKind::Block(BlockStmt { stmts }))
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

        let body = self.parse_block_statement();

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
            value = Some(self.parse_expression());
        }

        self.expect(TokenKind::SemiColon)?;

        Ok(ClassMember::Property(PropertyMember {
            name,
            ty,
            initializer: value,
            is_static,
        }))
    }

    pub fn parse_expression(&mut self) -> Expr {
        self.parse_logical_or()
    }

    fn parse_logical_or(&mut self) -> Expr {
        let mut expr = self.parse_logical_and();

        while let Some(tok) = self.peek() {
            if tok.kind == TokenKind::DoublePipe {
                self.consume();
                let right = self.parse_logical_and();
                expr = self.make_expr(ExprKind::Binary(BinaryExpr {
                    left: Box::new(expr),
                    op: BinaryOp::LogicalOr,
                    right: Box::new(right),
                }));
            } else {
                break;
            }
        }

        expr
    }

    fn parse_logical_and(&mut self) -> Expr {
        let mut expr = self.parse_equality();

        while let Some(tok) = self.peek() {
            if tok.kind == TokenKind::And {
                self.consume();
                let right = self.parse_equality();
                expr = self.make_expr(ExprKind::Binary(BinaryExpr {
                    left: Box::new(expr),
                    op: BinaryOp::LogicalAnd,
                    right: Box::new(right),
                }));
            } else {
                break;
            }
        }

        expr
    }

    fn parse_equality(&mut self) -> Expr {
        let mut expr = self.parse_relational();

        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::EqualsEquals | TokenKind::BangEquals => {
                    let op = if tok.kind == TokenKind::EqualsEquals {
                        BinaryOp::Equals
                    } else {
                        BinaryOp::NotEquals
                    };
                    self.consume();
                    let right = self.parse_relational();
                    expr = self.make_expr(ExprKind::Binary(BinaryExpr {
                        left: Box::new(expr),
                        op,
                        right: Box::new(right),
                    }));
                }
                _ => break,
            }
        }

        expr
    }

    fn parse_relational(&mut self) -> Expr {
        let mut expr = self.parse_additive();

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
                    let right = self.parse_additive();
                    expr = self.make_expr(ExprKind::Binary(BinaryExpr {
                        left: Box::new(expr),
                        op,
                        right: Box::new(right),
                    }));
                }
                _ => break,
            }
        }

        expr
    }

    fn parse_additive(&mut self) -> Expr {
        let mut expr = self.parse_multiplicative();

        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Plus | TokenKind::Minus => {
                    let op = if tok.kind == TokenKind::Plus {
                        BinaryOp::Plus
                    } else {
                        BinaryOp::Minus
                    };
                    self.consume();
                    let right = self.parse_multiplicative();
                    expr = self.make_expr(ExprKind::Binary(BinaryExpr {
                        left: Box::new(expr),
                        op,
                        right: Box::new(right),
                    }));
                }
                _ => break,
            }
        }

        expr
    }

    fn parse_multiplicative(&mut self) -> Expr {
        let mut expr = self.parse_unary();

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
                    let right = self.parse_unary();
                    expr = self.make_expr(ExprKind::Binary(BinaryExpr {
                        left: Box::new(expr),
                        op,
                        right: Box::new(right),
                    }));
                }
                _ => break,
            }
        }

        expr
    }

    fn parse_unary(&mut self) -> Expr {
        if let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Minus | TokenKind::Bang => {
                    let op = if tok.kind == TokenKind::Minus {
                        UnaryOp::Negate
                    } else {
                        UnaryOp::Not
                    };
                    self.consume();
                    let expr = self.parse_postfix();
                    return self.make_expr(ExprKind::Unary(UnaryExpr {
                        op,
                        expr: Box::new(expr),
                    }));
                }
                _ => {}
            }
        }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Expr {
        let mut expr = self.parse_primary();

        loop {
            match self.peek_kind() {
                Some(TokenKind::LeftParen) => {
                    self.consume();

                    let args = self.parse_argument_list();

                    if self.expect(TokenKind::RightParen).is_err() {
                        self.synchronize_expr();
                        return self.make_error_expr();
                    }

                    expr = self.make_expr(ExprKind::Call(CallExpr {
                        callee: Box::new(expr),
                        args,
                    }));
                }

                Some(TokenKind::RightArrow) | Some(TokenKind::Dot) => {
                    let is_static = matches!(self.peek_kind(), Some(TokenKind::Dot));
                    self.consume();

                    let field = match self.expect(TokenKind::Identifier) {
                        Ok(tok) => tok.content.unwrap(),
                        Err(err) => {
                            self.errors.push(err);
                            self.synchronize_expr();
                            return self.make_error_expr();
                        }
                    };

                    expr = self.make_expr(ExprKind::Access(AccessExpr {
                        object: Box::new(expr),
                        field,
                        is_static,
                    }));
                }

                _ => break,
            }
        }

        expr
    }

    fn parse_primary(&mut self) -> Expr {
        if let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::New => {
                    self.consume();
                    let obj = Box::new(self.parse_expression());
                    return self.make_expr(ExprKind::Construction(ConstructionExpr { obj }));
                }
                TokenKind::Number => {
                    let value = tok.content.unwrap();
                    if value.contains('.') {
                        let float_value = value.parse::<f64>().unwrap();
                        self.consume();
                        return self.make_expr(ExprKind::Literal(Literal::Float(float_value)));
                    } else {
                        let int_value = value.parse::<i64>().unwrap();
                        self.consume();
                        return self.make_expr(ExprKind::Literal(Literal::Int(int_value)));
                    }
                }
                TokenKind::String => {
                    let value = tok.content.unwrap();
                    self.consume();
                    return self.make_expr(ExprKind::Literal(Literal::String(value)));
                }
                TokenKind::Identifier => {
                    if tok.content.as_deref() == Some("true") {
                        self.consume();
                        return self.make_expr(ExprKind::Literal(Literal::Boolean(true)));
                    } else if tok.content.as_deref() == Some("false") {
                        self.consume();
                        return self.make_expr(ExprKind::Literal(Literal::Boolean(false)));
                    }
                    let name = tok.content.unwrap();
                    self.consume();
                    return self.make_expr(ExprKind::Variable(name));
                }
                TokenKind::LeftParen => {
                    self.consume();
                    let expr = self.parse_expression();
                    if let Err(err) = self.expect(TokenKind::RightParen) {
                        self.errors.push(err);
                        self.synchronize_expr();
                        return self.make_error_expr();
                    }
                    return self.make_expr(ExprKind::Grouping(GroupingExpr {
                        expr: Box::new(expr),
                    }));
                }
                _ => {}
            }
        }

        self.errors.push(ParseError::UnexpectedToken {
            expected: Expected::Expression("expression"),
            found: self.peek().unwrap_or_else(|| Token {
                pos: token::Position { idx: 0, len: 0 },
                kind: TokenKind::EoF,
                content: Some("EOF".to_string()),
            }),
        });
        self.consume();

        self.make_error_expr()
    }

    fn parse_argument_list(&mut self) -> Vec<Expr> {
        let mut args = Vec::new();

        if let Some(tok) = self.peek()
            && tok.kind == TokenKind::RightParen
        {
            return args;
        }

        loop {
            args.push(self.parse_expression());

            if let Some(ntok) = self.peek()
                && ntok.kind == TokenKind::Comma
            {
                self.consume();
                continue;
            }

            break;
        }

        args
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
                _ => Err(ParseError::UnexpectedToken {
                    expected: Expected::Member,
                    found: ctok,
                }),
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

    pub fn parse(&mut self) -> Result<Program, Vec<ParseError>> {
        while let Some(tok) = self.peek() {
            let item = match tok.kind {
                TokenKind::Import => self.parse_import(),
                TokenKind::Class => self.parse_class(),
                TokenKind::EoF => break,
                _ => {
                    self.errors.push(ParseError::UnexpectedToken {
                        expected: Expected::Item,
                        found: tok,
                    });
                    return Err(self.errors.clone());
                }
            };

            match item {
                Ok(parsed_item) => self.out.items.push(parsed_item),
                Err(err) => self.errors.push(err),
            }
        }

        if !self.errors.is_empty() {
            return Err(self.errors.clone());
        }

        Ok(self.out.clone())
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::*, lex::lex, parse::Parser};

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

        let mut parser = Parser::new(toks);
        let ast = parser.parse().unwrap();
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

        let mut parser = Parser::new(toks);
        let ast = parser.parse().unwrap();
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

        let mut parser = Parser::new(toks);
        let ast = parser.parse().unwrap();
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

        let mut parser = Parser::new(toks);
        let ast = parser.parse().unwrap();
        assert_class(&ast, "Main", 1);
    }

    #[test]
    fn test_with_object_and_access() {
        let toks = lex("
            class Main {
                static main (argc: int, args: Array) : int {
                    Object obj = new Object;
                    obj->method();
                    obj->property = 42.0;
                }
            }
        "
        .to_string());

        let mut parser = Parser::new(toks);
        let ast = parser.parse().unwrap();
        let class = assert_class(&ast, "Main", 1);

        let method = &class.members[0];
        if let ClassMember::Method(MethodMember { body, .. }) = method {
            let stmts = match &body.kind {
                StmtKind::Block(BlockStmt { stmts }) => stmts,
                _ => panic!("Expected method body to be a block"),
            };
            assert_eq!(stmts.len(), 3);

            if let StmtKind::VarDecl(VarDeclStmt {
                name,
                ty,
                initializer,
            }) = &stmts[0].kind
            {
                assert_eq!(name, "obj");
                assert!(matches!(ty, Type::Named(t) if t == "Object"));
                if let Some(init) = initializer {
                    if let ExprKind::Construction(ConstructionExpr { obj }) = &init.kind {
                        if let ExprKind::Variable(name) = &obj.kind {
                            assert_eq!(name, "Object");
                        } else {
                            panic!("Expected object construction to reference 'Object'");
                        }
                    } else {
                        panic!("Expected initializer to be object construction");
                    }
                } else {
                    panic!("Expected initializer for object declaration");
                }
            } else {
                panic!("Expected first statement to be object declaration");
            }

            if let StmtKind::Expr(expr) = &stmts[1].kind {
                if let ExprKind::Call(CallExpr { callee, .. }) = &expr.kind {
                    if let ExprKind::Access(AccessExpr {
                        object,
                        field,
                        is_static: _,
                    }) = &callee.kind
                    {
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

            if let StmtKind::Assignment(AssignmentStmt { target, value }) = &stmts[2].kind {
                if let ExprKind::Access(AccessExpr {
                    object,
                    field,
                    is_static: _,
                }) = &target.kind
                {
                    if let ExprKind::Variable(name) = &object.kind {
                        assert_eq!(name, "obj");
                        assert_eq!(field, "property");
                    } else {
                        panic!("Expected object to be variable 'obj'");
                    }
                } else {
                    panic!("Expected assignment target to be access");
                }

                if let ExprKind::Literal(Literal::Float(num)) = &value.kind {
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
