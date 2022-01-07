use std::collections::HashMap;
use crate::parser;
use crate::define;

#[derive(Debug, Clone)]
pub struct Interpreter {
    pub context: Context<Value>,
    pub version: &'static str,
}

#[derive(Debug, Clone)]
pub struct Context<T> {
    pub scope: Vec<(usize, ScopeType)>,
    pub env: Vec<HashMap<String, T>>,
}
#[derive(Debug, Clone)]
pub enum ScopeType {
    Block,
    UnTyped,
    Function,
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

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            context: Context::<Value>::new(),
            version: define::VERSION,
        }
    }
}

impl<T> Context<T> {
    pub fn new() -> Self {
        let scope = vec![(0, ScopeType::Block)];
        let env = vec![HashMap::new()];
        Context {
            scope: scope,
            env: env,
        }
    }
    pub fn push_new_scope(&mut self, scope_type: ScopeType) {
        let scope_number = self.env.len();
        self.scope.push((scope_number, scope_type));
        self.env.push(HashMap::new());
    }
    pub fn pop_scope(&mut self) {
        self.scope.pop();
        self.env.pop();
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
            if self.env[scope].contains_key(name) {
                return self.env[scope].get_mut(name);
            }
            if n == 0 {
                return None
            }
            n -= 1;
        }
    }
}
