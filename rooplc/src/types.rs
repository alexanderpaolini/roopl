use std::collections::HashMap;

use crate::ast;

#[derive(Debug, Clone, PartialEq)]
pub struct ClassType {
    pub name: String,
    pub base: Option<String>,

    pub static_members: HashMap<String, Type>,
    pub nonstatic_members: HashMap<String, Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    DuplicateClass(String),
    DuplicateMethod { class: String, method: String },
    DuplicateField { class: String, field: String },
    Internal(String),

    UndefinedVariable(String),
    UndefinedClass(String),
    UndefinedField { class: String, field: String },

    CyclicInheritance(Vec<String>),
    TypeMismatch { expected: Type, found: Type },
    InvalidOperation(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Void,
    Boolean,
    Class(String),
    Object(String),
    MethodSig {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Error,
}

impl Type {
    pub fn new(ast_type: ast::Type) -> Type {
        match ast_type {
            ast::Type::Int => Type::Int,
            ast::Type::Float => Type::Float,
            ast::Type::String => Type::String,
            ast::Type::Void => Type::Void,
            ast::Type::Boolean => Type::Boolean,
            ast::Type::Named(name) => Type::Object(name),
            ast::Type::Error => Type::Error,
        }
    }
}
