use std::collections::HashMap;
use crate::parser;

pub mod infer;

#[derive(Debug, Clone)]
pub struct Interpreter {
    pub context: Context<Value>,
    pub version: &'static str,
}

#[derive(Debug, Clone)]
pub struct Context<T> {
    pub scope: Vec<(usize, ScopeType)>,
    pub variables: Vec<HashMap<String, T>>,
}
#[derive(Debug, Clone)]
pub enum ScopeType {
    Block,
    UnTyped,
}

#[derive(Debug, Clone)]
pub enum Value {
    Ident(String),
    Bool(bool),
    Int(i64),
    Real(f64),
    String(String),
    Fn(parser::exp::Exp),
}

impl<T> Context<T> {
    pub fn push_new_scope(&mut self, scope_type: ScopeType) {
        let scope_number = self.variables.len();
        self.scope.push((scope_number, scope_type));
        self.variables.push(HashMap::new());
    }
    pub fn pop_scope(&mut self) {
        self.scope.pop();
        self.variables.pop();
    }
    pub fn current_scope(&self) -> &(usize, ScopeType) {
        &self.scope[self.scope.len() - 1]
    }

    pub fn search_identifier(&mut self, name: &str) -> Option<&mut T> {
        if self.scope.is_empty() {
            return None
        }
        let mut n = self.scope.len() - 1;
        loop {
            let scope = self.scope[n].0;
            if self.variables[scope].contains_key(name) {
                return self.variables[scope].get_mut(name);
            }
            if n == 0 {
                return None
            }
            n -= 1;
        }
    }
}
