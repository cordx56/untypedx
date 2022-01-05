use nom::{
    branch::{alt, permutation},
    bytes::complete::{escaped_transform, is_not, tag, take_while_m_n},
    character::complete::{char, line_ending, multispace0, multispace1, none_of, space0, space1},
    combinator::{all_consuming, map, opt, value},
    error::VerboseError,
    multi::{many0, many1},
    number::complete::double,
    sequence::{delimited, tuple},
    IResult,
};

use super::Parser;
use crate::define;

#[derive(Debug, Clone)]
pub enum Ty {
    Name(String),
    Tuple(Vec<Ty>),
    Fn(Box<Ty>, Box<Ty>),
}

impl Parser {
    pub fn ty(&self) -> impl FnMut(&str) -> IResult<&str, Ty, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                tuple((
                    alt((
                        map(self.ident_except_infopr(), |ident| Ty::Name(ident)),
                        map(
                            tuple((
                                tag(define::EXPRESSION_OPEN),
                                multispace0,
                                self.ty(),
                                multispace0,
                                tag(define::EXPRESSION_CLOSE),
                            )),
                            |(_, _, ty, _, _)| ty,
                        ),
                        map(
                            tuple((
                                tag(define::TUPLE_OPEN),
                                multispace0,
                                self.ty(),
                                multispace0,
                                tag(define::TUPLE_DELIMITER),
                                multispace0,
                                tag(define::TUPLE_CLOSE),
                            )),
                            |(_, _, ty, _, _, _, _)| Ty::Tuple(vec![ty]),
                        ),
                        map(
                            tuple((
                                tag(define::TUPLE_OPEN),
                                multispace0,
                                self.ty(),
                                many1(map(
                                    tuple((
                                        multispace0,
                                        tag(define::TUPLE_DELIMITER),
                                        multispace0,
                                        self.ty(),
                                    )),
                                    |(_, _, _, ty)| ty,
                                )),
                                multispace0,
                                tag(define::TUPLE_CLOSE),
                            )),
                            |(_, _, ty, ty_vec, _, _)| {
                                let mut tuple_vec = vec![ty];
                                for v in ty_vec {
                                    tuple_vec.push(v);
                                }
                                Ty::Tuple(tuple_vec)
                            },
                        ),
                    )),
                    opt(map(
                        tuple((space0, tag(define::FUNCTION_TYPE_ARROW), space0, self.ty())),
                        |(_, _, _, ty)| ty,
                    )),
                )),
                |(ty, function_ty_opt)| {
                    if let Some(function_ty) = function_ty_opt {
                        Ty::Fn(Box::new(ty), Box::new(function_ty))
                    } else {
                        ty
                    }
                },
            )(s)
        }
    }
}
