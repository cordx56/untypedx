use crate::parser::exp::Exp;
use crate::parser::cons::Cons;
use crate::interpreter::Context;

#[derive(Debug, Clone)]
pub enum Type {
    // primitive types
    Int(Option<i64>),
    Real(Option<f64>),
    String(Option<String>),

    Fn(Box<Type>, Box<Type>),
    UserDefined(String),
    Variable(String),
    Any,
    Union(Vec<Type>),
    Tuple(Vec<Type>),
} 

pub trait TypeInference {
    fn infer(&mut self, exp: &Exp) -> Type;
}

pub struct Inferer {
    pub context: Context<Type>,
}

impl TypeInference for Inferer {
    fn infer(&mut self, exp: &Exp) -> Type {
        match exp {
            Exp::Cons(cons) => {
                match cons {
                    Cons::Int(value) => {
                        Type::Int(Some(*value))
                    }
                    Cons::Real(value) => {
                        Type::Real(Some(*value))
                    }
                    Cons::String(value) => {
                        Type::String(Some(value.to_owned()))
                    }
                }
            }
            Exp::Ident(ident) => {
                match self.context.search_identifier(ident) {
                    Some(ident_type) => ident_type.clone(),
                    None => Type::Any,
                }
            }
            _ => Type::Any,
        }
    }
}
