pub mod builtin;

use std::collections::HashMap;

use crate::interpreter::{Context, ScopeType};
use crate::parser::cons::Cons;
use crate::parser::exp::{Exp, Pattern};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // primitive types
    Bool(Option<bool>),
    Int(Option<i64>),
    Real(Option<f64>),
    String(Option<String>),

    Fn(Box<Type>, Box<Type>),
    UserDefined(String),
    Any,
    Union(Vec<Type>),
    Tuple(Vec<Type>),

    // Type variable
    Variable {
        id: usize,
        name: Option<String>,
        instance: Option<Box<Type>>,
    },
}

pub struct Inferer {
    pub context: Context<Type>,
    pub next_type_variable_id: usize,
}

impl Inferer {
    pub fn new() -> Self {
        let mut inferer = Inferer {
            context: Context::new(),
            next_type_variable_id: 0,
        };
        inferer.init();
        inferer
    }
}

impl Inferer {
    pub fn new_type_variable(&mut self) -> Type {
        let ty = Type::Variable {
            id: self.next_type_variable_id,
            name: None,
            instance: None,
        };
        self.next_type_variable_id += 1;
        ty
    }
    pub fn get_type(&mut self, ident: &str, non_generic: &[Type]) -> Result<Type, String> {
        if let Some(ty) = self.context.search_identifier(ident) {
            let cloned = ty.clone();
            Ok(self.fresh(&cloned, non_generic))
        } else {
            Err(format!("{} is undefined", ident))
        }
    }
    pub fn freshrec(
        &mut self,
        t: &Type,
        non_generic: &[Type],
        mappings: &mut HashMap<usize, Type>,
    ) -> Type {
        let pruned = prune(t);
        match &pruned {
            Type::Variable { id, .. } => {
                if is_generic(&pruned, non_generic) {
                    if !mappings.contains_key(id) {
                        mappings.insert(*id, self.new_type_variable());
                    }
                    mappings[id].clone()
                } else {
                    pruned
                }
            }
            Type::Fn(arg, ret) => Type::Fn(
                Box::new(self.freshrec(arg, non_generic, mappings)),
                Box::new(self.freshrec(ret, non_generic, mappings)),
            ),
            Type::Union(type_vec) => {
                let mut result = Vec::new();
                for ty in type_vec {
                    result.push(self.freshrec(ty, non_generic, mappings));
                }
                Type::Union(result)
            }
            Type::Tuple(type_vec) => {
                let mut result = Vec::new();
                for ty in type_vec {
                    result.push(self.freshrec(ty, non_generic, mappings));
                }
                Type::Union(result)
            }
            _ => pruned,
        }
    }
    pub fn fresh(&mut self, t: &Type, non_generic: &[Type]) -> Type {
        let mut mappings = HashMap::new();
        return self.freshrec(t, non_generic, &mut mappings);
    }

    pub fn analyze(&mut self, exp: &Exp, non_generic: &[Type]) -> Result<Type, String> {
        match exp {
            Exp::Cons(cons) => match cons {
                Cons::Int(value) => Ok(Type::Int(Some(*value))),
                Cons::Real(value) => Ok(Type::Real(Some(*value))),
                Cons::String(value) => Ok(Type::String(Some(value.to_owned()))),
            },
            Exp::Ident(ident) => match self.get_type(ident, non_generic) {
                Ok(ident_type) => Ok(ident_type.clone()),
                Err(_) => Ok(Type::Any),
            },
            Exp::Tuple(exp_vec) => {
                let mut result = Vec::new();
                for v in exp_vec {
                    let x = self.analyze(v, non_generic)?;
                    result.push(x);
                }
                Ok(Type::Tuple(result))
            }

            Exp::App(func, arg) => {
                let func_type = self.analyze(func, non_generic)?;
                let arg_type = self.analyze(arg, non_generic)?;
                let ret_type = self.new_type_variable();
                match unify(
                    &Type::Fn(Box::new(arg_type), Box::new(ret_type)),
                    &func_type,
                ) {
                    Ok(fn_type) => match fn_type {
                        Type::Fn(_, ret_type) => Ok(*ret_type),
                        _ => Err("not a function type".to_owned()),
                    },
                    Err(e) => Err(e),
                }
            }

            Exp::Inf(arg1, infopr, arg2) => {
                let func_type =  self.get_type(&infopr.opr, non_generic)?;
                let arg1_type = self.analyze(arg1, non_generic)?;
                let mid_ret_type = self.new_type_variable();
                let mid_fn_type = unify(
                    &Type::Fn(Box::new(arg1_type), Box::new(mid_ret_type)),
                    &func_type,
                )?;
                let mid_ret_type;
                match mid_fn_type {
                    Type::Fn(_, ret_type) => mid_ret_type = ret_type,
                    _ => return Err("Not a function type".to_owned()),
                }
                let arg2_type = self.analyze(arg2, non_generic)?;
                let ret_type = self.new_type_variable();
                match unify(
                    &Type::Fn(Box::new(arg2_type), Box::new(ret_type)),
                    &mid_ret_type,
                ) {
                    Ok(fn_type) => match fn_type {
                        Type::Fn(_, ret_type) => Ok(*ret_type),
                        _ => Err("Not a function type".to_owned()),
                    },
                    Err(e) => Err(e),
                }
            }

            Exp::Fn(func_vec) => {
                self.context.push_new_scope(ScopeType::Function);
                let mut union_vec = Vec::new();
                for func in func_vec {
                    let mut new_non_generic = non_generic.to_vec();
                    let mut arg_types = Vec::new();
                    for pat in &func.0 {
                        match pat {
                            Pattern::Wildcard => {}
                            Pattern::Ident(ident) => {
                                let last_scope_index = self.context.env.len() - 1;
                                let arg_type = self.new_type_variable();
                                new_non_generic.push(arg_type.clone());
                                arg_types.push(arg_type.clone());
                                self.context.env[last_scope_index]
                                    .insert(ident.to_owned(), arg_type);
                            }
                            _ => return Err("Not implemented".to_owned()),
                        }
                    }
                    let ret_type = self.analyze(&func.1, &new_non_generic)?;

                    let mut fn_type = ret_type;
                    for v in arg_types.iter().rev() {
                        fn_type = Type::Fn(Box::new(v.clone()), Box::new(fn_type));
                    }
                    union_vec.push(fn_type);
                }
                self.context.pop_scope();
                if union_vec.len() == 1 {
                    Ok(union_vec[0].clone())
                } else {
                    Ok(Type::Union(union_vec))
                }
            }

            Exp::TypeAnnotated(exp, ty) => Err("Not implemented".to_owned()),
            _ => Ok(Type::Any),
        }
    }
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
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Bool(v_opt) => match v_opt {
                Some(v) => write!(f, "bool({})", v),
                None => write!(f, "bool"),
            },
            Type::Int(v_opt) => match v_opt {
                Some(v) => write!(f, "int({})", v),
                None => write!(f, "int"),
            },
            Type::Real(v_opt) => match v_opt {
                Some(v) => write!(f, "real({})", v),
                None => write!(f, "real"),
            },
            Type::String(v_opt) => match v_opt {
                Some(v) => write!(f, "string({})", v),
                None => write!(f, "string"),
            },
            Type::Fn(arg, ret) => write!(f, "({} -> {})", arg, ret),
            Type::Union(union_vec) => {
                write!(f, "(");
                for i in 0..union_vec.len() {
                    write!(f, "{}", union_vec[i])?;
                    if i != union_vec.len() - 1 {
                        write!(f, " | ")?;
                    }
                }
                write!(f, ")")
            }
            Type::Tuple(tuple_vec) => {
                write!(f, "(");
                for i in 0..tuple_vec.len() {
                    write!(f, "{}", tuple_vec[i])?;
                    if i != tuple_vec.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            _ => write!(f, "{:?}", self),
        }
    }
}

pub fn prune(ty: &Type) -> Type {
    if let Type::Variable { instance, .. } = ty {
        if let Some(v) = instance {
            return prune(v);
        }
    }
    return ty.clone();
}

pub fn unify(ty1: &Type, ty2: &Type) -> Result<Type, String> {
    let a = prune(ty1);
    let b = prune(ty2);
    match &a {
        Type::Variable { id, name, .. } => {
            if a != b {
                if occurs_in_type(&a, &b) {
                    return Err("Recursive Unification".to_owned());
                }
            }
            Ok(b)
        }
        _ => match b {
            Type::Variable { .. } => return unify(&b, &a),
            _ => match (&a, &b) {
                (Type::Bool(va), Type::Bool(vb)) => {
                    if va == vb {
                        Ok(Type::Bool(*va))
                    } else {
                        Ok(Type::Bool(None))
                    }
                }
                (Type::Int(va), Type::Int(vb)) => {
                    if va == vb {
                        Ok(Type::Int(*va))
                    } else {
                        Ok(Type::Int(None))
                    }
                }
                (Type::Real(va), Type::Real(vb)) => {
                    if va == vb {
                        Ok(Type::Real(*va))
                    } else {
                        Ok(Type::Real(None))
                    }
                }
                (Type::String(va), Type::String(vb)) => {
                    if va == vb {
                        Ok(Type::String(va.to_owned()))
                    } else {
                        Ok(Type::String(None))
                    }
                }
                (Type::Fn(arga, reta), Type::Fn(argb, retb)) => {
                    let arg_unified = unify(&arga, &argb)?;
                    let ret_unified = unify(&reta, &retb)?;
                    Ok(Type::Fn(Box::new(arg_unified), Box::new(ret_unified)))
                }
                (Type::Union(va_vec), Type::Union(vb_vec)) => {
                    if va_vec.len() != vb_vec.len() {
                        Err(format!("Type mismatch {} != {}", a, b))
                    } else {
                        let mut res_vec = Vec::new();

                        for i in 0..va_vec.len() {
                            let res = unify(&va_vec[i], &vb_vec[i])?;
                            res_vec.push(res);
                        }

                        Ok(Type::Union(res_vec))
                    }
                }
                (Type::Tuple(va_vec), Type::Tuple(vb_vec)) => {
                    if va_vec.len() != vb_vec.len() {
                        Err(format!("Type mismatch {} != {}", a, b))
                    } else {
                        let mut res_vec = Vec::new();

                        for i in 0..va_vec.len() {
                            let res = unify(&va_vec[i], &vb_vec[i])?;
                            res_vec.push(res);
                        }

                        Ok(Type::Tuple(res_vec))
                    }
                }
                (Type::Any, _) => Ok(Type::Any),
                (_, Type::Any) => Ok(Type::Any),
                (_, _) => Err(format!("Type mismatch {} != {}", a, b)),
            },
        },
    }
}

pub fn occurs_in_type(tyvar: &Type, tosearch: &Type) -> bool {
    let tosearch_pruned = prune(tosearch);
    if &tosearch_pruned == tyvar {
        return true;
    } else if tosearch_pruned.is_type_operator() {
        return occurs_in(tyvar, &tosearch_pruned);
    }
    return false;
}
pub fn occurs_in(tyvar: &Type, tosearch: &Type) -> bool {
    match tosearch {
        Type::Fn(arg, ret) => occurs_in_type(tyvar, arg) || occurs_in_type(tyvar, ret),
        Type::Union(type_vec) => {
            for ty in type_vec {
                if occurs_in_type(tyvar, ty) {
                    return true;
                }
            }
            false
        }
        Type::Tuple(type_vec) => {
            for ty in type_vec {
                if occurs_in_type(tyvar, ty) {
                    return true;
                }
            }
            false
        }
        _ => false,
    }
}

pub fn is_generic(t: &Type, non_generic: &[Type]) -> bool {
    match non_generic.iter().find(|x| x == &t) {
        Some(_) => false,
        None => true,
    }
}
