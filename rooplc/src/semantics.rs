use std::collections::HashMap;

use crate::ast::*;

#[derive(Debug, Clone, PartialEq)]
pub struct MethodSig {
    pub params: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassType {
    pub name: String,
    pub base: Option<String>,

    pub static_methods: HashMap<String, MethodSig>,
    pub instance_methods: HashMap<String, MethodSig>,

    pub static_properties: HashMap<String, Type>,
    pub instance_properties: HashMap<String, Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    DuplicateClass(String),
    DuplicateMethod { class: String, method: String },
    DuplicateField { class: String, field: String },
    Internal(String),
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

    pub fn process(&mut self, ast: &Program) -> TypeEnv {
        for item in &ast.items {
            self.process_item(item);
        }

        self.clone()
    }

    fn process_item(&mut self, item: &Item) {
        match item {
            Item::Import(_) => self.errors.push(TypeError::Internal(
                "unexpected import statement in typechecking phase".to_string(),
            )),
            Item::Class(class_decl) => self.process_class(class_decl),
        }
    }

    fn process_class(&mut self, class: &ClassDecl) {
        if self.classes.contains_key(&class.name) {
            self.errors
                .push(TypeError::DuplicateClass(class.name.clone()));
            return;
        }

        let mut class_type = ClassType {
            name: class.name.clone(),
            base: class.base.clone(),
            static_methods: HashMap::new(),
            static_properties: HashMap::new(),
            instance_methods: HashMap::new(),
            instance_properties: HashMap::new(),
        };

        for member in &class.members {
            self.process_member(member, &mut class_type);
        }

        self.classes.insert(class.name.clone(), class_type);
    }

    fn process_member(&mut self, member: &ClassMember, class_type: &mut ClassType) {
        match member {
            ClassMember::Property(prop) => self.process_property(prop, class_type),
            ClassMember::Method(meth) => self.process_method(meth, class_type),
        }
    }

    fn process_property(&mut self, prop: &PropertyMember, class_type: &mut ClassType) {
        if prop.is_static {
            if class_type.static_properties.contains_key(&prop.name) {
                self.errors.push(TypeError::DuplicateField {
                    class: class_type.name.clone(),
                    field: prop.name.clone(),
                });
                return;
            }
            class_type
                .static_properties
                .insert(prop.name.clone(), prop.ty.clone());
        } else {
            if class_type.instance_properties.contains_key(&prop.name) {
                self.errors.push(TypeError::DuplicateField {
                    class: class_type.name.clone(),
                    field: prop.name.clone(),
                });
                return;
            }
            class_type
                .instance_properties
                .insert(prop.name.clone(), prop.ty.clone());
        }
    }

    fn process_method(&mut self, meth: &MethodMember, class_type: &mut ClassType) {
        let sig = MethodSig {
            params: meth.params.iter().map(|p| p.ty.clone()).collect(),
            return_type: meth.return_type.clone(),
        };

        if meth.is_static {
            if class_type.static_methods.contains_key(&meth.name) {
                self.errors.push(TypeError::DuplicateMethod {
                    class: class_type.name.clone(),
                    method: meth.name.clone(),
                });
                return;
            }
            class_type.static_methods.insert(meth.name.clone(), sig);
        } else {
            if class_type.instance_methods.contains_key(&meth.name) {
                self.errors.push(TypeError::DuplicateMethod {
                    class: class_type.name.clone(),
                    method: meth.name.clone(),
                });
                return;
            }
            class_type.instance_methods.insert(meth.name.clone(), sig);
        }
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

pub fn check(ast: Program) -> Result<(), Vec<TypeError>> {
    let te = TypeEnv::new().process(&ast);

    if te.errors.len() > 0 {
        return Err(te.errors);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lex, parse};

    fn gen_ast(code: String) -> Program {
        parse::parse(lex::lex(code))
    }

    #[test]
    fn test_empty_program() {
        let prog = gen_ast("".to_string());
        let mut tc = TypeEnv::new();
        let env = tc.process(&prog);

        assert!(env.classes.is_empty());
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
        let env = tc.process(&prog);

        let class = env.classes.get("A").unwrap();
        assert!(class.static_properties.contains_key("x"));
        assert!(class.instance_properties.contains_key("y"));
        assert!(class.static_methods.contains_key("foo"));
        assert!(class.instance_methods.contains_key("bar"));
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
}
