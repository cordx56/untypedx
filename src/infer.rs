pub mod builtin;

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

pub trait TypeInference {
    fn new_type_variable(&mut self) -> Type;
    fn analyze(&mut self, exp: &Exp) -> Result<Type, String>;
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

impl TypeInference for Inferer {
    fn new_type_variable(&mut self) -> Type {
        let ty = Type::Variable {
            id: self.next_type_variable_id,
            name: None,
            instance: None,
        };
        self.next_type_variable_id += 1;
        ty
    }
    fn analyze(&mut self, exp: &Exp) -> Result<Type, String> {
        match exp {
            Exp::Cons(cons) => match cons {
                Cons::Int(value) => Ok(Type::Int(Some(*value))),
                Cons::Real(value) => Ok(Type::Real(Some(*value))),
                Cons::String(value) => Ok(Type::String(Some(value.to_owned()))),
            },
            Exp::Ident(ident) => match self.context.search_identifier(ident) {
                Some(ident_type) => Ok(ident_type.clone()),
                None => Ok(Type::Any),
            },
            Exp::Tuple(exp_vec) => {
                let mut result = Vec::new();
                for v in exp_vec {
                    let x = self.analyze(v)?;
                    result.push(x);
                }
                Ok(Type::Tuple(result))
            }

            Exp::App(func, arg) => {
                let func_type = self.analyze(func)?;
                let arg_type = self.analyze(arg)?;
                let result_type = self.new_type_variable();
                match unify(
                    &Type::Fn(Box::new(arg_type), Box::new(result_type)),
                    &func_type,
                ) {
                    Ok((fn_type, _)) => match fn_type {
                        Type::Fn(_, result_type) => Ok(*result_type),
                        _ => Err("not a function type".to_owned()),
                    },
                    Err(e) => Err(e),
                }
            }

            Exp::Inf(arg1, infopr, arg2) => {
                let func_type;
                match self.context.search_identifier(&infopr.opr) {
                    Some(ident_type) => func_type = ident_type.clone(),
                    None => return Err(format!("Infix function {} not found", infopr.opr)),
                }
                let arg1_type = self.analyze(arg1)?;
                let mid_ret_type = self.new_type_variable();
                let (mid_fn_type, _) = unify(
                    &Type::Fn(Box::new(arg1_type), Box::new(mid_ret_type)),
                    &func_type,
                )?;
                let arg2_type = self.analyze(arg2)?;
                let ret_type = self.new_type_variable();
                match unify(
                    &Type::Fn(Box::new(arg2_type), Box::new(ret_type)),
                    &mid_fn_type,
                ) {
                    Ok((fn_type, _)) => match fn_type {
                        Type::Fn(_, ret_type) => Ok(*ret_type),
                        _ => Err("not a function type".to_owned()),
                    },
                    Err(e) => Err(e),
                }
            }

            Exp::Fn(func_vec) => {
                self.context.push_new_scope(ScopeType::Function);
                let mut union_vec = Vec::new();
                for func in func_vec {
                    let arg_type = self.new_type_variable();
                    for pat in &func.0 {
                        match pat {
                            Pattern::Wildcard => {}
                            Pattern::Ident(ident) => {
                                let last_scope_index = self.context.env.len() - 1;
                                let new_type_var = self.new_type_variable();
                                self.context.env[last_scope_index]
                                    .insert(ident.to_owned(), new_type_var);
                            }
                            _ => return Err("Not implemented".to_owned()),
                        }
                    }
                    let ret_type = self.analyze(&func.1)?;
                    union_vec.push(ret_type);
                }
                self.context.pop_scope();
                if union_vec.len() == 1 {
                    Ok(union_vec[0].clone())
                } else {
                    Ok(Type::Union(union_vec))
                }
            }

            Exp::TypeAnnotated(exp, ty) => Err("not implemented".to_owned()),
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
            return *(v.clone());
        }
    }
    return ty.clone();
}

pub fn unify(ty1: &Type, ty2: &Type) -> Result<(Type, Type), String> {
    let a = prune(ty1);
    let b = prune(ty2);
    match &a {
        Type::Variable { id, name, .. } => {
            if a != b {
                if occurs_in_type(&a, &b) {
                    return Err("Recursive Unification".to_owned());
                }
            }
            let new_a = Type::Variable {
                id: *id,
                name: name.clone(),
                instance: Some(Box::new(b.clone())),
            };
            Ok((new_a, b))
        }
        _ => match b {
            Type::Variable { .. } => return unify(&b, &a),
            _ => match (&a, &b) {
                (Type::Bool(va), Type::Bool(vb)) => {
                    //if va != vb {
                    //    Err(format!("Static value type mismatch: {} != {}", a, b))
                    //} else {
                    Ok((Type::Bool(*va), Type::Bool(*vb)))
                    //}
                }
                (Type::Int(va), Type::Int(vb)) => Ok((Type::Int(*va), Type::Int(*vb))),
                (Type::Real(va), Type::Real(vb)) => Ok((Type::Real(*va), Type::Real(*vb))),
                (Type::String(va), Type::String(vb)) => {
                    Ok((Type::String(va.to_owned()), Type::String(vb.to_owned())))
                }
                (Type::Fn(arga, reta), Type::Fn(argb, retb)) => {
                    let arg_unified = unify(&arga, &argb)?;
                    let ret_unified = unify(&reta, &retb)?;
                    Ok((
                        Type::Fn(Box::new(arg_unified.0), Box::new(ret_unified.0)),
                        Type::Fn(Box::new(arg_unified.1), Box::new(ret_unified.1)),
                    ))
                }
                (Type::Union(va_vec), Type::Union(vb_vec)) => {
                    if va_vec.len() != vb_vec.len() {
                        Err(format!("Type mismatch {} != {}", a, b))
                    } else {
                        let mut res_vec_a = Vec::new();
                        let mut res_vec_b = Vec::new();

                        for i in 0..va_vec.len() {
                            let res = unify(&va_vec[i], &vb_vec[i])?;
                            res_vec_a.push(res.0);
                            res_vec_b.push(res.1);
                        }

                        Ok((Type::Union(res_vec_a), Type::Union(res_vec_b)))
                    }
                }
                (Type::Tuple(va_vec), Type::Tuple(vb_vec)) => {
                    if va_vec.len() != vb_vec.len() {
                        Err(format!("Type mismatch {} != {}", a, b))
                    } else {
                        let mut res_vec_a = Vec::new();
                        let mut res_vec_b = Vec::new();

                        for i in 0..va_vec.len() {
                            let res = unify(&va_vec[i], &vb_vec[i])?;
                            res_vec_a.push(res.0);
                            res_vec_b.push(res.1);
                        }

                        Ok((Type::Union(res_vec_a), Type::Union(res_vec_b)))
                    }
                }
                (Type::Any, _) => Ok((Type::Any, Type::Any)),
                (_, Type::Any) => Ok((Type::Any, Type::Any)),
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
