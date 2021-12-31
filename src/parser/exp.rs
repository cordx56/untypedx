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
use super::stmt::Stmts;
use super::{InfOpr, Parser};
use crate::define;

#[derive(Debug, Clone)]
pub enum Exp {
    Cons(Cons),
    Ident(String),
    UnTyped(Box<Exp>),
    Block(Stmts),

    App(Box<Exp>, Box<Exp>),
    Inf(Box<Exp>, InfOpr, Box<Exp>),

    RegInfix(String),
}

impl Parser {
    pub fn atexp(&self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        |s: &str| {
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
            ))(s)
        }
    }
    pub fn appexp(&self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                tuple((
                    self.atexp(),
                    many0(map(tuple((multispace1, self.atexp())), |(_, exp)| exp)),
                )),
                |(exp, exp_vec)| {
                    if 0 < exp_vec.len() {
                        let mut result = Exp::App(Box::new(exp), Box::new(exp_vec[0].clone()));
                        for i in 1..exp_vec.len() {
                            result = Exp::App(Box::new(result), Box::new(exp_vec[i].clone()));
                        }
                        result
                    } else {
                        exp
                    }
                },
            )(s)
        }
    }
    pub fn infexp(&self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                tuple((
                    self.appexp(),
                    many0(map(
                        tuple((multispace0, self.infopr(), multispace0, self.appexp())),
                        |(_, infopr, _, infexp)| (infopr, infexp),
                    )),
                )),
                |(appexp, opr_exp_tuple_vec)| self.rebuild_tree(appexp, opr_exp_tuple_vec),
            )(s)
        }
    }
    pub fn exp(&self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        self.infexp()
    }
    pub fn untypedexp(&self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                tuple((tag(define::UNTYPED), multispace0, self.exp())),
                |(_, _, exp)| Exp::UnTyped(Box::new(exp)),
            )(s)
        }
    }

    pub fn reginfixexp(
        &mut self,
    ) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                tuple((
                    tag(define::INFIX),
                    multispace1,
                    self.real(),
                    multispace1,
                    self.ident_except_infopr(),
                )),
                |(_, _, d, _, identifier)| {
                    match d {
                        Cons::Real(v) => {
                            self.push_infix(true, v as usize, identifier.clone());
                        }
                        _ => {}
                    }
                    Exp::RegInfix(identifier)
                },
            )(s)
        }
    }
}
