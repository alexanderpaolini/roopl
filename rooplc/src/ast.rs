use std::fmt;

use crate::token::TokenKind;

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
        is_static: bool,
    },
    Method {
        name: String,
        params: Vec<Parameter>,
        return_type: Type,
        body: Block,
        is_static: bool,
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

    Expr(Expr),
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: TokenKind,
        found: TokenKind,
    },
    EndOfInput,
    InvalidExpression,
    NotImplemented,
}

fn indent_lines(s: &str, indent: &str) -> String {
    s.lines()
        .map(|line| format!("{}{}", indent, line))
        .collect::<Vec<_>>()
        .join("\n")
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Program")?;
        for item in &self.items {
            let item_str = format!("{item}");
            writeln!(f, "{}", indent_lines(&item_str, "  "))?;
        }
        Ok(())
    }
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::Import(import_stmt) => write!(f, "{}", import_stmt),
            Item::Class(class_decl) => write!(f, "{}", class_decl),
        }
    }
}

impl fmt::Display for ClassDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "Class {}{}",
            self.name,
            self.base
                .as_ref()
                .map_or(String::new(), |base| format!(" (extends {})", base))
        )?;
        for member in &self.members {
            let member_str = format!("{}", member);
            writeln!(f, "{}", indent_lines(&member_str, "  "))?;
        }
        Ok(())
    }
}

impl fmt::Display for ImportStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Import {}", self.path.join("."))
    }
}

impl fmt::Display for ClassMember {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClassMember::Property {
                name,
                ty,
                initializer,
                is_static,
            } => {
                if *is_static {
                    write!(f, "static ")?;
                }
                write!(f, "{} : {:?}", name, ty)?;
                if let Some(init) = initializer {
                    write!(f, " = {:?}", init)?;
                }
                Ok(())
            }
            ClassMember::Method {
                name,
                params,
                return_type,
                body,
                is_static,
            } => {
                if *is_static {
                    write!(f, "static ")?;
                }
                write!(f, "{} (", name)?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {:?}", param.name, param.ty)?;
                }
                writeln!(f, ") : {:?} ", return_type)?;
                writeln!(f, "{}", indent_lines(&format!("{:?}", body), "  "))
            }
        }
    }
}
