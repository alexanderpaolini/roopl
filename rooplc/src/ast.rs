use std::fmt;

use crate::token::{Token, TokenKind};

pub type ExprId = u32;
pub type StmtId = u32;
pub type ClassId = u32;

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub id: ExprId,
    pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal(Literal),
    Variable(String),
    Construction(ConstructionExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Grouping(GroupingExpr),
    Call(CallExpr),
    Access(AccessExpr),
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub op: BinaryOp,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GroupingExpr {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstructionExpr {
    pub obj: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AccessExpr {
    pub is_static: bool,
    pub object: Box<Expr>,
    pub field: String,
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
    Boolean(bool),
    Int(i64),
    Float(f64),
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
    pub id: ClassId,
    pub name: String,
    pub base: Option<String>,
    pub members: Vec<ClassMember>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassMember {
    Property(PropertyMember),
    Method(MethodMember),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PropertyMember {
    pub name: String,
    pub ty: Type,
    pub initializer: Option<Expr>,
    pub is_static: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodMember {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub body: Stmt,
    pub is_static: bool,
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
    Boolean,
    Named(String),
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub id: StmtId,
    pub kind: StmtKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Block(BlockStmt),
    VarDecl(VarDeclStmt),
    Assignment(AssignmentStmt),
    If(IfStmt),
    For(ForStmt),
    Return(ReturnStmt),
    Expr(Expr),
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclStmt {
    pub ty: Type,
    pub name: String,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentStmt {
    pub target: Expr,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub init: Option<Box<Stmt>>,
    pub condition: Expr,
    pub update: Option<Box<Stmt>>,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken { expected: Expected, found: Token },
    EndOfInput,
    InvalidExpression,
    NotImplemented,
}

#[derive(Debug, Clone)]
pub enum Expected {
    Token(TokenKind),
    Member,
    Item,
    Expression(&'static str),
    Statement(&'static str),
    Type(&'static str),
    Identifier(&'static str),
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
            writeln!(f, "{}", indent_lines(&format!("{}", item), "  "))?;
        }
        Ok(())
    }
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::Import(import) => write!(f, "{}", import),
            Item::Class(class) => write!(f, "{}", class),
        }
    }
}

impl fmt::Display for ImportStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Import {}", self.path.join("."))
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
                .map_or(String::new(), |b| format!(" (extends {})", b))
        )?;
        for member in &self.members {
            writeln!(f, "{}", indent_lines(&format!("{}", member), "  "))?;
        }
        Ok(())
    }
}

impl fmt::Display for ClassMember {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClassMember::Property(p) => {
                if p.is_static {
                    write!(f, "static ")?;
                }
                write!(f, "{} : {:?}", p.name, p.ty)?;
                if let Some(init) = &p.initializer {
                    write!(f, " = {}", init)?;
                }
                Ok(())
            }
            ClassMember::Method(m) => {
                if m.is_static {
                    write!(f, "static ")?;
                }
                write!(f, "{}(", m.name)?;
                for (i, param) in m.params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {:?}", param.name, param.ty)?;
                }
                writeln!(f, ") : {:?}", m.return_type)?;
                writeln!(f, "{}", indent_lines(&format!("{}", m.body), "  "))
            }
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            StmtKind::Block(b) => writeln!(f, "Block\n{}", b),
            StmtKind::VarDecl(v) => {
                write!(f, "{} : {:?}", v.name, v.ty)?;
                if let Some(init) = &v.initializer {
                    write!(f, " = {}", init)?;
                }
                writeln!(f)
            }
            StmtKind::Assignment(a) => writeln!(f, "{} = {}", a.target, a.value),
            StmtKind::If(i) => {
                writeln!(f, "if {}", i.condition)?;
                writeln!(f, "{}", indent_lines(&format!("{}", i.then_branch), "  "))?;
                if let Some(e) = &i.else_branch {
                    writeln!(f, "else")?;
                    writeln!(f, "{}", indent_lines(&format!("{}", e), "  "))?;
                }
                Ok(())
            }
            StmtKind::For(fstmt) => {
                writeln!(f, "for")?;
                if let Some(init) = &fstmt.init {
                    writeln!(f, "{}", indent_lines(&format!("{}", init), "  "))?;
                }
                writeln!(f, "{}", indent_lines(&format!("{}", fstmt.condition), "  "))?;
                if let Some(update) = &fstmt.update {
                    writeln!(f, "{}", indent_lines(&format!("{}", update), "  "))?;
                }
                writeln!(f, "{}", indent_lines(&format!("{}", fstmt.body), "    "))
            }
            StmtKind::Return(r) => {
                if let Some(val) = &r.value {
                    writeln!(f, "return {}", val)
                } else {
                    writeln!(f, "return")
                }
            }
            StmtKind::Expr(e) => writeln!(f, "{}", e),
            StmtKind::Error => writeln!(f, "ERROR"),
        }
    }
}

impl fmt::Display for BlockStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in &self.stmts {
            writeln!(f, "{}", indent_lines(&format!("{}", stmt), "  "))?;
        }
        Ok(())
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ExprKind::Construction(c) => write!(f, "new {:?}", c),
            ExprKind::Literal(l) => write!(f, "{:?}", l),
            ExprKind::Variable(n) => write!(f, "{}", n),
            ExprKind::Unary(u) => write!(f, "({:?} {})", u.op, u.expr),
            ExprKind::Binary(b) => write!(f, "({} {:?} {})", b.left, b.op, b.right),
            ExprKind::Grouping(g) => write!(f, "(group {})", g.expr),
            ExprKind::Call(c) => {
                write!(f, "{}(", c.callee)?;
                for (i, arg) in c.args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            ExprKind::Access(a) => {
                if a.is_static {
                    write!(f, "{}.{}", a.object, a.field)
                } else {
                    write!(f, "{}->{}", a.object, a.field)
                }
            }
            ExprKind::Error => write!(f, "ERROR"),
        }
    }
}
