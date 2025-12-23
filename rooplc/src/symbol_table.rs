use std::collections::HashMap;

use crate::types::Type;

#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, Type>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn push(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop(&mut self) {
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
