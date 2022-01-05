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

use super::cons::Cons;
use super::infix::InfOpr;
use super::stmt::Stmts;
use super::ty::Ty;
use super::Parser;
use crate::define;

#[derive(Debug, Clone)]
pub enum Exp {
    Cons(Cons),
    Ident(String),
    Tuple(Vec<Exp>),
    UnTyped(Box<Exp>),
    Block(Stmts),

    App(Box<Exp>, Box<Exp>),
    Inf(Box<Exp>, InfOpr, Box<Exp>),

    Fn(Vec<(Fn)>),

    TypeAnnotated(Box<Exp>, Ty),
}

#[derive(Debug, Clone)]
pub struct Fn(Vec<Pattern>, Exp);

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Cons(Cons),
    Ident(String),
    Tuple(Vec<Pattern>),
    TypeAnnotated(Box<Pattern>, Ty),
}

impl Parser {
    pub fn atexp(&self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        let mut clone = self.clone();
        move |s: &str| {
            alt((
                map(self.cons(), |cons| Exp::Cons(cons)),
                map(self.ident_except_infopr(), |ident| Exp::Ident(ident)),
                map(
                    tuple((
                        tag(define::EXPRESSION_OPEN),
                        multispace0,
                        self.exp(),
                        multispace0,
                        tag(define::EXPRESSION_CLOSE),
                    )),
                    |(_, _, exp, _, _)| exp,
                ),
                map(
                    tuple((
                        tag(define::TUPLE_OPEN),
                        multispace0,
                        self.exp(),
                        multispace0,
                        tag(define::TUPLE_DELIMITER),
                        multispace0,
                        tag(define::TUPLE_CLOSE),
                    )),
                    |(_, _, exp, _, _, _, _)| Exp::Tuple(vec![exp]),
                ),
                map(
                    tuple((
                        tag(define::TUPLE_OPEN),
                        multispace0,
                        self.exp(),
                        many1(map(
                            tuple((
                                multispace0,
                                tag(define::TUPLE_DELIMITER),
                                multispace0,
                                self.exp(),
                            )),
                            |(_, _, _, exp)| exp,
                        )),
                        multispace0,
                        tag(define::TUPLE_CLOSE),
                    )),
                    |(_, _, exp, exp_vec, _, _)| {
                        let mut result = vec![exp];
                        for v in exp_vec {
                            result.push(v);
                        }
                        Exp::Tuple(result)
                    },
                ),
                clone.blockexp(),
                self.fnexp(),
            ))(s)
        }
    }
    pub fn appexp(&self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        move |s: &str| {
            let (s, exp) = self.atexp()(s)?;
            let (s, exp_vec) = many0(map(tuple((space1, self.atexp())), |(_, exp)| exp))(s)?;
            if 0 < exp_vec.len() {
                let mut result = Exp::App(Box::new(exp), Box::new(exp_vec[0].clone()));
                for i in 1..exp_vec.len() {
                    result = Exp::App(Box::new(result), Box::new(exp_vec[i].clone()));
                }
                Ok((s, result))
            } else {
                Ok((s, exp))
            }
        }
    }
    pub fn infexp(&self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                tuple((
                    self.appexp(),
                    many0(map(
                        tuple((space0, self.infopr(), space0, self.appexp())),
                        |(_, infopr, _, infexp)| (infopr, infexp),
                    )),
                )),
                |(appexp, opr_exp_tuple_vec)| self.rebuild_tree(appexp, opr_exp_tuple_vec),
            )(s)
        }
    }
    pub fn exp(&self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                tuple((
                    alt((self.infexp(), self.untypedexp())),
                    opt(map(
                        tuple((space0, tag(define::TYPE_ANNOTATION), space0, self.ty())),
                        |(_, _, _, ty)| ty,
                    )),
                )),
                |(exp, ty_opt)| {
                    if let Some(ty) = ty_opt {
                        Exp::TypeAnnotated(Box::new(exp), ty)
                    } else {
                        exp
                    }
                },
            )(s)
        }
    }

    pub fn untypedexp(&self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                tuple((tag(define::UNTYPED), space0, self.exp())),
                |(_, _, exp)| Exp::UnTyped(Box::new(exp)),
            )(s)
        }
    }
    pub fn blockexp(&mut self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                tuple((
                    tag(define::BLOCK_OPEN),
                    multispace0,
                    self.stmts(),
                    multispace0,
                    tag(define::BLOCK_CLOSE),
                )),
                |(_, _, stmts, _, _)| Exp::Block(stmts),
            )(s)
        }
    }

    pub fn fnexp(&self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        move |s: &str| {
            map(
                tuple((
                    tag(define::FN),
                    multispace1,
                    self.fnrule(),
                    many0(map(
                        tuple((multispace0, tag(define::FN_OR), multispace0, self.fnrule())),
                        |(_, _, _, fnrule)| fnrule,
                    )),
                )),
                |(_, _, fnrule, fnrule_vec)| {
                    let mut result = vec![fnrule];
                    for v in fnrule_vec {
                        result.push(v);
                    }
                    Exp::Fn(result)
                },
            )(s)
        }
    }

    pub fn fnrule(
        &self,
    ) -> impl FnMut(&str) -> IResult<&str, Fn, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                tuple((
                    self.fnpats(),
                    multispace0,
                    tag(define::FN_ARROW),
                    multispace0,
                    self.exp(),
                )),
                |(fnpats, _, _, _, exp)| Fn(fnpats, exp),
            )(s)
        }
    }
    pub fn fnpats(
        &self,
    ) -> impl FnMut(&str) -> IResult<&str, Vec<Pattern>, VerboseError<&str>> + '_ {
        |s: &str| many0(map(tuple((multispace0, self.pat())), |(_, pat)| pat))(s)
    }
    pub fn pat(&self) -> impl FnMut(&str) -> IResult<&str, Pattern, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                tuple((
                    self.atpat(),
                    opt(map(
                        tuple((space0, tag(define::TYPE_ANNOTATION), space0, self.ty())),
                        |(_, _, _, ty)| ty,
                    )),
                )),
                |(pat, ty_opt)| match ty_opt {
                    Some(ty) => Pattern::TypeAnnotated(Box::new(pat), ty),
                    None => pat,
                },
            )(s)
        }
    }
    pub fn atpat(&self) -> impl FnMut(&str) -> IResult<&str, Pattern, VerboseError<&str>> + '_ {
        |s: &str| {
            alt((
                map(tag(define::PAT_WILDCARD), |_| Pattern::Wildcard),
                map(self.cons(), |cons| Pattern::Cons(cons)),
                map(self.ident_except_infopr(), |ident| Pattern::Ident(ident)),
                map(
                    tuple((
                        tag(define::TUPLE_OPEN),
                        multispace0,
                        tag(define::TUPLE_CLOSE),
                    )),
                    |_| Pattern::Tuple(Vec::new()),
                ),
                map(
                    tuple((
                        tag(define::EXPRESSION_OPEN),
                        multispace0,
                        self.pat(),
                        multispace0,
                        tag(define::EXPRESSION_CLOSE),
                    )),
                    |(_, _, pat, _, _)| pat,
                ),
                map(
                    tuple((
                        tag(define::TUPLE_OPEN),
                        multispace0,
                        self.pat(),
                        multispace0,
                        tag(define::TUPLE_DELIMITER),
                        multispace0,
                        tag(define::TUPLE_CLOSE),
                    )),
                    |(_, _, pat, _, _, _, _)| Pattern::Tuple(vec![pat]),
                ),
                map(
                    tuple((
                        tag(define::TUPLE_OPEN),
                        multispace0,
                        self.pat(),
                        many1(map(
                            tuple((
                                multispace0,
                                tag(define::TUPLE_DELIMITER),
                                multispace0,
                                self.pat(),
                            )),
                            |(_, _, _, pat)| pat,
                        )),
                        multispace0,
                        tag(define::TUPLE_CLOSE),
                    )),
                    |(_, _, pat, pat_vec, _, _)| {
                        let mut result = vec![pat];
                        for v in pat_vec {
                            result.push(v);
                        }
                        Pattern::Tuple(result)
                    },
                ),
            ))(s)
        }
    }
}
