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
    InvalidOverride { class: String, method: String },
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
            ast::Type::Named(name) => Type::Class(name),
            ast::Type::Error => Type::Error,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeEnv {
    classes: HashMap<String, ClassType>,
    errors: Vec<TypeError>,
}

impl TypeEnv {
    fn new() -> TypeEnv {
        Self {
            classes: HashMap::new(),
            errors: vec![],
        }
    }

    pub fn process(&mut self, ast: &ast::Program) -> Result<(), Vec<TypeError>> {
        for item in &ast.items {
            self.process_item(item);
        }

        if !self.errors.is_empty() {
            return Err(self.errors.clone());
        }

        Ok(())
    }

    fn process_item(&mut self, item: &ast::Item) {
        match item {
            ast::Item::Import(_) => self.errors.push(TypeError::Internal(
                "unexpected import statement in typechecking phase".to_string(),
            )),
            ast::Item::Class(class_decl) => self.process_class(class_decl),
        }
    }

    fn process_class(&mut self, class: &ast::ClassDecl) {
        if self.classes.contains_key(&class.name) {
            self.errors
                .push(TypeError::DuplicateClass(class.name.clone()));
            return;
        }

        let mut class_type = ClassType {
            name: class.name.clone(),
            base: class.base.clone(),
            nonstatic_members: HashMap::new(),
            static_members: HashMap::new(),
        };

        if let Some(base_name) = &class.base {
            class_type
                .nonstatic_members
                .insert("super".to_string(), Type::Class(base_name.clone()));
        }

        for member in &class.members {
            self.process_member(member, &mut class_type);
        }

        self.classes.insert(class.name.clone(), class_type);
    }

    fn process_member(&mut self, member: &ast::ClassMember, class_type: &mut ClassType) {
        match member {
            ast::ClassMember::Property(prop) => self.process_property(prop, class_type),
            ast::ClassMember::Method(meth) => self.process_method(meth, class_type),
        }
    }

    fn process_property(&mut self, prop: &ast::PropertyMember, class_type: &mut ClassType) {
        let members = if prop.is_static {
            &mut class_type.static_members
        } else {
            &mut class_type.nonstatic_members
        };

        if members.contains_key(&prop.name) {
            self.errors.push(TypeError::DuplicateField {
                class: class_type.name.clone(),
                field: prop.name.clone(),
            });
            return;
        }
        members.insert(prop.name.clone(), Type::new(prop.ty.clone()));
    }

    fn process_method(&mut self, meth: &ast::MethodMember, class_type: &mut ClassType) {
        let sig = Type::MethodSig {
            params: meth
                .params
                .iter()
                .map(|p| Type::new(p.ty.clone()))
                .collect(),
            return_type: Box::new(Type::new(meth.return_type.clone())),
        };

        let props = if meth.is_static {
            &mut class_type.static_members
        } else {
            &mut class_type.nonstatic_members
        };

        if props.contains_key(&meth.name) {
            self.errors.push(TypeError::DuplicateMethod {
                class: class_type.name.clone(),
                method: meth.name.clone(),
            });
            return;
        }
        props.insert(meth.name.clone(), sig);
    }

    fn print_errors(&self) -> Result<(), ()> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            for error in &self.errors {
                println!("{:?}", error);
            }
            Err(())
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, Type>>,
}

impl SymbolTable {
    fn new() -> SymbolTable {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    fn push(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop(&mut self) {
        if self.scopes.pop().is_none() {
            panic!("No scope to remove!");
        }
    }

    pub fn insert(&mut self, name: String, ty: Type) {
        if let Some(sc) = self.scopes.last_mut() {
            sc.insert(name, ty);
        } else {
            panic!("No scope to insert to!")
        }
    }

    pub fn get(&self, name: &str) -> Type {
        for sc in self.scopes.iter().rev() {
            if let Some(ty) = sc.get(name) {
                return ty.clone();
            }
        }

        Type::Error
    }
}

#[derive(Debug)]
pub struct TypeCheck {
    errors: Vec<TypeError>,
    te: TypeEnv,
    st: SymbolTable,
}

impl TypeCheck {
    fn new(te: TypeEnv) -> Self {
        Self {
            errors: vec![],
            te,
            st: SymbolTable::new(),
        }
    }

    fn process_class(&mut self, class: &ast::ClassDecl) {
        for member in &class.members {
            match member {
                ast::ClassMember::Property(property_member) => {
                    self.process_property_member(property_member)
                }
                ast::ClassMember::Method(method_member) => {
                    self.process_method_member(method_member, &class.name)
                }
            }
        }
    }

    fn process_property_member(&mut self, prop: &ast::PropertyMember) {
        if let Some(initializer) = &prop.initializer {
            let initializer_type = self.check_expr(initializer);
            let ty = Type::new(prop.ty.clone());
            if initializer_type != ty {
                self.errors.push(TypeError::TypeMismatch {
                    expected: ty,
                    found: initializer_type,
                });
            }
        }
    }

    fn process_method_member(&mut self, meth: &ast::MethodMember, class_name: &str) {
        self.st.push();
        self.st
            .insert("this".to_string(), Type::Class(class_name.to_string()));

        if let Some(class_type) = self.te.classes.get(class_name) {
            if let Some(base) = &class_type.base {
                self.st
                    .insert("super".to_string(), Type::Class(base.clone()));
            }
        }

        for param in &meth.params {
            self.st
                .insert(param.name.clone(), Type::new(param.ty.clone()));
        }

        self.check_stmt(&meth.body);

        self.st.pop();
    }

    fn check_stmt(&mut self, stmt: &ast::Stmt) {
        match &stmt.kind {
            ast::StmtKind::VarDecl(ast::VarDeclStmt {
                ty,
                name,
                initializer,
            }) => {
                let ty = Type::new(ty.clone());
                if let Some(init_expr) = initializer {
                    let init_type = self.check_expr(init_expr);
                    if init_type != ty {
                        self.errors.push(TypeError::TypeMismatch {
                            expected: ty.clone(),
                            found: init_type,
                        });
                    }
                }
                self.st.insert(name.clone(), ty);
            }
            ast::StmtKind::Expr(expr) => {
                self.check_expr(expr);
            }
            ast::StmtKind::Block(ast::BlockStmt { stmts }) => {
                self.st.push();
                for stmt in stmts {
                    self.check_stmt(stmt);
                }
                self.st.pop();
            }
            ast::StmtKind::If(ast::IfStmt {
                condition,
                then_branch,
                else_branch,
            }) => {
                let cond_type = self.check_expr(condition);
                if cond_type != Type::Int {
                    self.errors.push(TypeError::InvalidOperation(
                        "if condition must be a boolean".to_string(),
                    ));
                }
                self.check_stmt(then_branch);
                if let Some(else_stmt) = else_branch {
                    self.check_stmt(else_stmt);
                }
            }
            ast::StmtKind::Return(ast::ReturnStmt { value }) => {
                if let Some(return_expr) = value {
                    self.check_expr(return_expr);
                }
            }
            ast::StmtKind::Assignment(ast::AssignmentStmt { target, value }) => {
                let target_type = self.check_expr(target);
                let value_type = self.check_expr(value);

                if target_type != value_type {
                    self.errors.push(TypeError::TypeMismatch {
                        expected: target_type,
                        found: value_type,
                    });
                }
            }
            ast::StmtKind::For(ast::ForStmt {
                init,
                condition,
                update,
                body,
            }) => {
                self.st.push();

                if let Some(init_stmt) = init {
                    self.check_stmt(init_stmt);
                }

                let cond_type = self.check_expr(condition);
                if cond_type != Type::Boolean {
                    self.errors.push(TypeError::InvalidOperation(
                        "for loop condition must be a boolean".to_string(),
                    ));
                }

                if let Some(inc_expr) = update {
                    self.check_stmt(inc_expr);
                }

                self.check_stmt(body);

                self.st.pop();
            }
            ast::StmtKind::Error => {
                self.errors.push(TypeError::Internal(
                    "encountered an error statement during type checking".to_string(),
                ));
            }
        }
    }

    pub fn check_expr(&mut self, expr: &ast::Expr) -> Type {
        match &expr.kind {
            ast::ExprKind::Literal(lit) => self.check_literal(lit),
            ast::ExprKind::Variable(name) => {
                let ty = self.st.get(name);
                if ty == Type::Error {
                    if self.te.classes.contains_key(name) {
                        return Type::Class(name.clone());
                    }
                    self.errors.push(TypeError::UndefinedVariable(name.clone()));
                }
                ty
            }
            ast::ExprKind::Unary(unary) => self.check_unary(unary),
            ast::ExprKind::Binary(binary) => self.check_binary(binary),
            ast::ExprKind::Grouping(ast::GroupingExpr { expr }) => self.check_expr(expr),
            ast::ExprKind::Call(call) => self.check_call(call),
            ast::ExprKind::Access(access) => self.check_access(access),
            ast::ExprKind::Error => Type::Error,
            ast::ExprKind::Construction(construction) => self.check_construction(construction),
        }
    }

    fn check_literal(&self, lit: &ast::Literal) -> Type {
        match lit {
            ast::Literal::Boolean(_) => Type::Boolean,
            ast::Literal::Int(_) => Type::Int,
            ast::Literal::Float(_) => Type::Float,
            ast::Literal::String(name) => {
                if let Some(class_type) = self.te.classes.get(name) {
                    Type::Class(class_type.name.clone())
                } else {
                    self.st.get(name)
                }
            }
        }
    }

    fn check_unary(&mut self, unary: &ast::UnaryExpr) -> Type {
        let expr_type = self.check_expr(&unary.expr);
        match unary.op {
            ast::UnaryOp::Not => {
                if expr_type != Type::Boolean {
                    self.errors.push(TypeError::InvalidOperation(
                        "logical NOT operator requires a boolean operand".into(),
                    ));
                    Type::Error
                } else {
                    Type::Boolean
                }
            }
            ast::UnaryOp::Negate => {
                if expr_type != Type::Int && expr_type != Type::Float {
                    self.errors.push(TypeError::InvalidOperation(
                        "negation operator requires an integer or float operand".into(),
                    ));
                    Type::Error
                } else {
                    expr_type
                }
            }
        }
    }

    fn check_binary(&mut self, binary: &ast::BinaryExpr) -> Type {
        let left_type = self.check_expr(&binary.left);
        let right_type = self.check_expr(&binary.right);

        use ast::BinaryOp::*;

        match binary.op {
            LogicalOr | LogicalAnd => self.check_logical_op(left_type, right_type),
            Equals | NotEquals => self.check_equality_op(left_type, right_type),
            LessThan | GreaterThan | LessThanEqualTo | GreaterThanEqualTo => {
                self.check_comparison_op(left_type, right_type)
            }
            Plus | Minus | Times | Divide | Mod => self.check_arithmetic_op(left_type, right_type),
        }
    }

    fn check_logical_op(&mut self, left: Type, right: Type) -> Type {
        if left != Type::Boolean || right != Type::Boolean {
            self.errors.push(TypeError::InvalidOperation(
                "logical operators require boolean operands".into(),
            ));
            Type::Error
        } else {
            Type::Boolean
        }
    }

    fn check_equality_op(&mut self, left: Type, right: Type) -> Type {
        if left != right {
            self.errors.push(TypeError::TypeMismatch {
                expected: left,
                found: right,
            });
            Type::Error
        } else {
            Type::Boolean
        }
    }

    fn check_comparison_op(&mut self, left: Type, right: Type) -> Type {
        if self.is_numeric(&left) && self.is_numeric(&right) {
            Type::Boolean
        } else {
            self.errors.push(TypeError::InvalidOperation(
                "comparison operators require numeric operands".into(),
            ));
            Type::Error
        }
    }

    fn check_arithmetic_op(&mut self, left: Type, right: Type) -> Type {
        if self.is_numeric(&left) && self.is_numeric(&right) {
            // Promote Int + Float to Float
            if left == Type::Float || right == Type::Float {
                Type::Float
            } else {
                Type::Int
            }
        } else {
            self.errors.push(TypeError::InvalidOperation(
                "arithmetic operators require numeric operands".into(),
            ));
            Type::Error
        }
    }

    fn is_numeric(&self, ty: &Type) -> bool {
        matches!(ty, Type::Int | Type::Float)
    }

    fn check_call(&mut self, call: &ast::CallExpr) -> Type {
        let callee_type = self.check_expr(&call.callee);

        if let Type::MethodSig {
            params,
            return_type,
        } = callee_type
        {
            if params.len() != call.args.len() {
                self.errors.push(TypeError::TypeMismatch {
                    expected: Type::Error,
                    found: Type::Error,
                });
                return Type::Error;
            }

            for (arg, param_type) in call.args.iter().zip(params.iter()) {
                let arg_type = self.check_expr(arg);
                if arg_type != *param_type {
                    self.errors.push(TypeError::TypeMismatch {
                        expected: param_type.clone(),
                        found: arg_type,
                    });
                }
            }

            *return_type
        } else {
            self.errors.push(TypeError::InvalidOperation(
                "attempted to call a non-callable type".into(),
            ));
            Type::Error
        }
    }

    fn check_access(&mut self, access: &ast::AccessExpr) -> Type {
        let object_type = self.check_expr(&access.object);
        if let Type::Class(class_name) = object_type {
            if let Some(class_type) = self.te.classes.get(&class_name) {
                let props = if access.is_static {
                    &class_type.static_members
                } else {
                    &class_type.nonstatic_members
                };

                println!(
                    "Accessing field '{}' in class '{}'",
                    access.field, class_name
                );

                if let Some(field_type) = props.get(&access.field) {
                    println!(
                        "Accessing field '{}' in class '{}'",
                        access.field, class_name
                    );
                    field_type.clone()
                } else {
                    self.errors.push(TypeError::UndefinedField {
                        class: class_name,
                        field: access.field.clone(),
                    });
                    Type::Error
                }
            } else {
                self.errors.push(TypeError::UndefinedClass(class_name));
                Type::Error
            }
        } else {
            self.errors.push(TypeError::InvalidOperation(
                "attempted to access a field on a non-class type".into(),
            ));
            Type::Error
        }
    }

    fn check_construction(&mut self, construction: &ast::ConstructionExpr) -> Type {
        return self.check_expr(&construction.obj);
    }

    fn process(&mut self, ast: &ast::Program) -> Result<(), Vec<TypeError>> {
        for item in &ast.items {
            match item {
                ast::Item::Class(class_decl) => self.process_class(class_decl),
                _ => {
                    self.errors.push(TypeError::Internal(
                        "unexpected import in preprocessed ast".to_string(),
                    ));
                }
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }
}

pub fn check(ast: ast::Program) -> Result<(), Vec<TypeError>> {
    let mut te = TypeEnv::new();
    te.process(&ast)?;

    let mut tc = TypeCheck::new(te);
    let x = tc.process(&ast);
    println!("{:#?}", tc);

    x
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lex, parse};
    use ast::*;

    fn gen_ast(code: String) -> Program {
        parse::Parser::new(lex::lex(code)).parse().unwrap()
    }

    #[test]
    fn test_empty_program() {
        let prog = gen_ast("".to_string());
        let mut tc = TypeEnv::new();
        let result = tc.process(&prog);

        assert!(result.is_ok());
        assert!(tc.classes.is_empty());
        assert!(tc.errors.is_empty());
    }

    #[test]
    fn test_single_class_with_members() {
        let prog = gen_ast(
            "
        class A {
            static x : int;
            y : float;
            static foo(x : int) : int {}
            bar(x : float) : void {}
        }
        "
            .to_string(),
        );

        let mut tc = TypeEnv::new();
        let result = tc.process(&prog);

        assert!(result.is_ok());
        let class = tc.classes.get("A").unwrap();
        assert!(class.static_members.contains_key("x"));
        assert!(class.nonstatic_members.contains_key("y"));
        assert!(class.static_members.contains_key("foo"));
        assert!(class.nonstatic_members.contains_key("bar"));
        assert!(tc.errors.is_empty());
    }

    #[test]
    fn test_duplicate_class_error() {
        let prog = gen_ast(
            "
        class A {}
        class A {}
        "
            .to_string(),
        );

        let mut tc = TypeEnv::new();
        let _ = tc.process(&prog);

        assert_eq!(tc.errors.len(), 1);
        match &tc.errors[0] {
            TypeError::DuplicateClass(name) => assert_eq!(name, "A"),
            _ => panic!("Expected DuplicateClass error"),
        }
    }

    #[test]
    fn test_duplicate_static_property_error() {
        let prog = gen_ast(
            "
        class A {
            static x : int;
            static x : int;
        }
        "
            .to_string(),
        );
        let mut tc = TypeEnv::new();
        let _ = tc.process(&prog);

        assert_eq!(tc.errors.len(), 1);
        match &tc.errors[0] {
            TypeError::DuplicateField { class: _, field } => assert_eq!(field, "x"),
            _ => panic!("Expected DuplicateStaticProperty error"),
        }
    }

    #[test]
    fn test_static_instance_same_name_allowed() {
        let prog = gen_ast(
            "
        class A {
            static x : int;
            x : float;
        }
        "
            .to_string(),
        );

        let mut tc = TypeEnv::new();
        let _ = tc.process(&prog);

        assert!(tc.errors.is_empty());
    }

    #[test]
    fn test_example_main_function() {
        let prog = gen_ast(
            "
        class TestObj {
            test () : int
            {
                return 0;
            }
        }

        class Main {
            static main() : int
            {
                return TestObj.test();
            }
        }
        "
            .to_string(),
        );

        let mut tc = TypeEnv::new();
        let _ = tc.process(&prog);

        assert!(tc.errors.is_empty());
    }
}
