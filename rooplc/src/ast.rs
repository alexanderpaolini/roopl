use crate::token::{Token, TokenKind};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Variable(String),

    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },

    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },

    Grouping(Box<Expr>),

    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },

    Access {
        object: Box<Expr>,
        field: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Not,
    Negate,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    LogicalOr,
    LogicalAnd,

    Equals,
    NotEquals,

    LessThan,
    GreaterThan,
    LessThanEqualTo,
    GreaterThanEqualTo,

    Plus,
    Minus,
    Times,
    Divide,
    Mod,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Import(ImportStmt),
    Class(ClassDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportStmt {
    pub path: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDecl {
    pub name: String,
    pub base: Option<String>,
    pub members: Vec<ClassMember>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassMember {
    Property {
        name: String,
        ty: Type,
        initializer: Option<Expr>,
    },
    Method {
        name: String,
        params: Vec<Parameter>,
        return_type: Type,
        body: Block,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Void,
    Named(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Block(Block),

    VarDecl {
        ty: Type,
        name: String,
        initializer: Option<Expr>,
    },

    Assignment {
        target: Expr,
        value: Expr,
    },

    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },

    For {
        init: Option<Box<Stmt>>,
        condition: Expr,
        update: Option<Box<Stmt>>,
        body: Box<Stmt>,
    },

    Return(Option<Expr>),

    ExprStmt(Expr),
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken { expected: TokenKind, found: TokenKind },
    EndOfInput,
    InvalidExpression,
}
