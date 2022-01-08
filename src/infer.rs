pub mod builtin;

use std::collections::{HashMap, HashSet};

use crate::interpreter::{Context, ScopeType};
use crate::parser::cons::Cons;
use crate::parser::exp::{Exp, Pattern};

type TypeId = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // primitive types
    Bool(Option<bool>),
    Int(Option<i64>),
    Real(Option<f64>),
    String(Option<String>),

    Fn(TypeId, TypeId),
    UserDefined(String),
    Any,
    Union(Vec<TypeId>),
    Tuple(Vec<TypeId>),

    // Type variable
    Variable {
        id: TypeId,
        name: Option<String>,
        instance: Option<TypeId>,
    },
}

impl Type {
    pub fn is_type_variable(&self) -> bool {
        match self {
            Type::Variable { .. } => true,
            _ => false,
        }
    }
    pub fn is_type_operator(&self) -> bool {
        !self.is_type_variable()
    }
    pub fn set_instance(&mut self, new_instance: TypeId) {
        match self {
            &mut Type::Variable {
                ref mut instance, ..
            } => {
                *instance = Some(new_instance);
            }
            _ => {}
        }
    }
}

pub struct Inferer {
    pub context: Context<TypeId>,
    pub types: Vec<Type>,
}

impl Inferer {
    pub fn new() -> Self {
        let mut inferer = Inferer {
            context: Context::new(),
            types: Vec::new(),
        };
        inferer.init();
        inferer
    }

    pub fn new_type_variable(&mut self) -> TypeId {
        let id = self.types.len();
        self.types.push(Type::Variable {
            id: id,
            name: None,
            instance: None,
        });
        id
    }
    pub fn push_new_type(&mut self, t: Type) -> TypeId {
        let id = self.types.len();
        self.types.push(t);
        id
    }
    pub fn get_type(
        &mut self,
        ident: &str,
        non_generic: &HashSet<TypeId>,
    ) -> Result<TypeId, String> {
        if let Some(tid) = self.context.search_identifier(ident) {
            let to_be_freshed = *tid;
            Ok(self.fresh(to_be_freshed, non_generic))
        } else {
            Err(format!("{} is undefined", ident))
        }
    }

    pub fn prune(&mut self, t: TypeId) -> TypeId {
        if let Type::Variable { instance, .. } = self.types[t].clone() {
            if let Some(v) = instance {
                let pruned = self.prune(v);
                if let Type::Variable {
                    ref mut instance, ..
                } = self.types[t]
                {
                    *instance = Some(pruned);
                }
                return pruned;
            }
        }
        t
    }
    pub fn freshrec(
        &mut self,
        t: TypeId,
        non_generic: &HashSet<TypeId>,
        mappings: &mut HashMap<TypeId, TypeId>,
    ) -> TypeId {
        let pruned = self.prune(t);
        match self.types[pruned].clone() {
            Type::Variable { .. } => {
                if self.is_generic(pruned, non_generic) {
                    mappings
                        .entry(pruned)
                        .or_insert(self.new_type_variable())
                        .clone()
                } else {
                    pruned
                }
            }
            Type::Fn(arg, ret) => {
                let arg_type = self.freshrec(arg, non_generic, mappings);
                let ret_type = self.freshrec(ret, non_generic, mappings);
                self.push_new_type(Type::Fn(arg_type, ret_type))
            }
            Type::Union(type_vec) => {
                let mut result = Vec::new();
                for ty in type_vec {
                    result.push(self.freshrec(ty, non_generic, mappings));
                }
                self.push_new_type(Type::Union(result))
            }
            Type::Tuple(type_vec) => {
                let mut result = Vec::new();
                for ty in type_vec {
                    result.push(self.freshrec(ty, non_generic, mappings));
                }
                self.push_new_type(Type::Union(result))
            }
            _ => pruned,
        }
    }
    pub fn fresh(&mut self, t: TypeId, non_generic: &HashSet<TypeId>) -> TypeId {
        let mut mappings = HashMap::new();
        return self.freshrec(t, non_generic, &mut mappings);
    }

    pub fn occurs_in_type(&mut self, tyvar: TypeId, tosearch: TypeId) -> bool {
        let tosearch_pruned = self.prune(tosearch);
        if tosearch_pruned == tyvar {
            return true;
        } else if self.types[tosearch_pruned].is_type_operator() {
            return self.occurs_in(tyvar, tosearch_pruned);
        }
        return false;
    }
    pub fn occurs_in(&mut self, tyvar: TypeId, tosearch: TypeId) -> bool {
        match self.types[tosearch].clone() {
            Type::Fn(arg, ret) => {
                self.occurs_in_type(tyvar, arg) || self.occurs_in_type(tyvar, ret)
            }
            Type::Union(type_vec) => {
                for ty in type_vec {
                    if self.occurs_in_type(tyvar, ty) {
                        return true;
                    }
                }
                false
            }
            Type::Tuple(type_vec) => {
                for ty in type_vec {
                    if self.occurs_in_type(tyvar, ty) {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }

    pub fn is_generic(&self, t: TypeId, non_generic: &HashSet<TypeId>) -> bool {
        !non_generic.contains(&t)
    }

    pub fn unify(&mut self, ty1: TypeId, ty2: TypeId) -> Result<(), String> {
        let a = self.prune(ty1);
        let b = self.prune(ty2);
        match (self.types[a].clone(), self.types[b].clone()) {
            (Type::Variable { .. }, _) => {
                if a != b {
                    if self.occurs_in_type(a, b) {
                        return Err("Recursive Unification".to_owned());
                    }
                    self.types[a].set_instance(b);
                }
                Ok(())
            }
            (_, Type::Variable { .. }) => self.unify(b, a),
            (Type::Bool(va), Type::Bool(vb)) => {
                if va != vb {
                    self.types[a] = Type::Bool(None);
                    self.types[b] = Type::Bool(None);
                }
                Ok(())
            }
            (Type::Int(va), Type::Int(vb)) => {
                if va != vb {
                    self.types[a] = Type::Int(None);
                    self.types[b] = Type::Int(None);
                }
                Ok(())
            }
            (Type::Real(va), Type::Real(vb)) => {
                if va != vb {
                    self.types[a] = Type::Real(None);
                    self.types[b] = Type::Real(None);
                }
                Ok(())
            }
            (Type::String(va), Type::String(vb)) => {
                if va != vb {
                    self.types[a] = Type::String(None);
                    self.types[b] = Type::String(None);
                }
                Ok(())
            }
            (Type::Fn(arga, reta), Type::Fn(argb, retb)) => {
                self.unify(arga, argb)?;
                self.unify(reta, retb)?;
                Ok(())
            }
            (Type::Union(va_vec), Type::Union(vb_vec)) => {
                // ?
                if va_vec.len() != vb_vec.len() {
                    Err(format!("Type mismatch {} != {}", a, b))
                } else {
                    for i in 0..va_vec.len() {
                        self.unify(va_vec[i], vb_vec[i])?;
                    }
                    Ok(())
                }
            }
            (Type::Union(va_vec), _) => {
                let mut types = Vec::new();
                for va in va_vec {
                    if let Ok(_) = self.unify(va, b) {
                        types.push(va);
                        println!("{}", self.display_type(va));
                    }
                }
                self.types[a] = Type::Union(types);
                Ok(())
            }
            (_, Type::Union(_)) => self.unify(b, a),
            (Type::Tuple(va_vec), Type::Tuple(vb_vec)) => {
                if va_vec.len() != vb_vec.len() {
                    Err(format!("Type mismatch {} != {}", a, b))
                } else {
                    for i in 0..va_vec.len() {
                        self.unify(va_vec[i], vb_vec[i])?;
                    }
                    Ok(())
                }
            }

            (Type::Any, _) => Ok(()),
            (_, Type::Any) => Ok(()),
            (_, _) => Err(format!(
                "Type mismatch {} != {}",
                self.display_type(a),
                self.display_type(b)
            )),
        }
    }
    pub fn analyze(&mut self, exp: &Exp, non_generic: &HashSet<TypeId>) -> Result<TypeId, String> {
        match exp {
            Exp::Cons(cons) => match cons {
                Cons::Int(value) => Ok(self.push_new_type(Type::Int(Some(*value)))),
                Cons::Real(value) => Ok(self.push_new_type(Type::Real(Some(*value)))),
                Cons::String(value) => Ok(self.push_new_type(Type::String(Some(value.to_owned())))),
            },
            Exp::Ident(ident) => match self.get_type(ident, non_generic) {
                Ok(tid) => Ok(tid),
                Err(_) => {
                    let new_type_variable = self.new_type_variable();
                    let last_scope_index = self.context.env.len() - 1;
                    self.context.env[last_scope_index].insert(ident.to_owned(), new_type_variable);
                    Ok(new_type_variable)
                }
            },
            Exp::Tuple(exp_vec) => {
                let mut result = Vec::new();
                for v in exp_vec {
                    let x = self.analyze(v, non_generic)?;
                    result.push(x);
                }
                Ok(self.push_new_type(Type::Tuple(result)))
            }

            Exp::App(func, arg) => {
                let func_type = self.analyze(func, non_generic)?;
                let arg_type = self.analyze(arg, non_generic)?;
                let ret_type = self.new_type_variable();
                let new_func_type = self.push_new_type(Type::Fn(arg_type, ret_type));
                println!("new_func_type: {}", self.display_type(new_func_type));
                self.unify(new_func_type, func_type)?;
                Ok(ret_type)
            }

            Exp::Inf(arg1, infopr, arg2) => {
                let func_type = self.get_type(&infopr.opr, non_generic)?;
                let arg1_type = self.analyze(arg1, non_generic)?;
                let mid_ret_type = self.new_type_variable();
                let mid_func_type = self.push_new_type(Type::Fn(arg1_type, mid_ret_type));
                self.unify(mid_func_type, func_type)?;
                let arg2_type = self.analyze(arg2, non_generic)?;
                let ret_type = self.new_type_variable();
                let new_func_type = self.push_new_type(Type::Fn(arg2_type, ret_type));
                self.unify(new_func_type, mid_ret_type)?;
                Ok(ret_type)
            }

            Exp::Fn(func_vec) => {
                self.context.push_new_scope(ScopeType::Function);
                let mut union_vec = Vec::new();
                for func in func_vec {
                    let mut new_non_generic = non_generic.clone();
                    let mut arg_types = Vec::new();
                    for pat in &func.0 {
                        match pat {
                            Pattern::Wildcard => {}
                            Pattern::Ident(ident) => {
                                let arg_type = self.new_type_variable();
                                new_non_generic.insert(arg_type);
                                arg_types.push(arg_type);
                                let last_scope_index = self.context.env.len() - 1;
                                self.context.env[last_scope_index]
                                    .insert(ident.to_owned(), arg_type);
                            }
                            _ => return Err("Not implemented".to_owned()),
                        }
                    }
                    let ret_type = self.analyze(&func.1, &new_non_generic)?;

                    let mut fn_type = ret_type;
                    for v in arg_types.iter().rev() {
                        fn_type = self.push_new_type(Type::Fn(*v, fn_type));
                    }
                    union_vec.push(fn_type);
                }
                self.context.pop_scope();
                if union_vec.len() == 1 {
                    Ok(union_vec[0])
                } else {
                    Ok(self.push_new_type(Type::Union(union_vec)))
                }
            }

            Exp::TypeAnnotated(exp, ty) => Err("Not implemented".to_owned()),
            _ => Ok(self.push_new_type(Type::Any)),
        }
    }

    pub fn display_type(&self, tid: TypeId) -> String {
        match &self.types[tid] {
            Type::Bool(v_opt) => match v_opt {
                Some(v) => format!("bool({})", v),
                None => "bool".to_owned(),
            },
            Type::Int(v_opt) => match v_opt {
                Some(v) => format!("int({})", v),
                None => "int".to_owned(),
            },
            Type::Real(v_opt) => match v_opt {
                Some(v) => format!("real({})", v),
                None => "real".to_owned(),
            },
            Type::String(v_opt) => match v_opt {
                Some(v) => format!("string({})", v),
                None => "string".to_owned(),
            },
            Type::Fn(arg, ret) => format!(
                "({} -> {})",
                self.display_type(*arg),
                self.display_type(*ret)
            ),
            Type::Union(type_vec) => {
                let mut res = "(".to_owned();
                for i in 0..type_vec.len() {
                    res.push_str(&self.display_type(type_vec[i]));
                    if i != type_vec.len() - 1 {
                        res.push_str(" | ");
                    }
                }
                res.push_str(")");
                res
            }
            Type::Tuple(type_vec) => {
                let mut res = "(".to_owned();
                for i in 0..type_vec.len() {
                    res.push_str(&self.display_type(type_vec[i]));
                    if i != type_vec.len() - 1 {
                        res.push_str(", ");
                    }
                }
                res.push_str(")");
                res
            }
            Type::Variable { id, instance, .. } => {
                if let Some(i) = instance {
                    self.display_type(*i)
                } else {
                    format!("'{}", id)
                }
            }
            _ => format!("{:?}", self.types[tid]),
        }
    }
}
