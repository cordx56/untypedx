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

    Fn(Vec<String>, Box<Exp>),

    RegInfix(InfOpr),
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
                self.blockexp(),
                self.fnexp(),
            ))(s)
        }
    }
    pub fn appexp(&self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                tuple((
                    self.atexp(),
                    many0(map(tuple((space1, self.atexp())), |(_, exp)| exp)),
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
                        tuple((space0, self.infopr(), space0, self.appexp())),
                        |(_, infopr, _, infexp)| (infopr, infexp),
                    )),
                )),
                |(appexp, opr_exp_tuple_vec)| self.rebuild_tree(appexp, opr_exp_tuple_vec),
            )(s)
        }
    }
    pub fn exp(&self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        move |s: &str| alt((self.infexp(), self.untypedexp()))(s)
    }

    pub fn untypedexp(&self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                tuple((tag(define::UNTYPED), space0, self.exp())),
                |(_, _, exp)| Exp::UnTyped(Box::new(exp)),
            )(s)
        }
    }
    pub fn blockexp(&self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
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
                    many0(map(
                        tuple((space1, self.ident_except_infopr())),
                        |(_, ident)| ident,
                    )),
                    space0,
                    tag(define::FN_ARROW),
                    space0,
                    self.exp(),
                )),
                |(_, args, _, _, _, exp)| Exp::Fn(args, Box::new(exp)),
            )(s)
        }
    }

    pub fn reginfexp(&mut self) -> impl FnMut(&str) -> IResult<&str, Exp, VerboseError<&str>> + '_ {
        let clone = self.clone();
        move |s: &str| {
            let ident_except_infopr = clone.ident_except_infopr();
            map(
                tuple((
                    alt((
                        map(tag(define::INFIX), |_| true),
                        map(tag(define::INFIXR), |_| false),
                    )),
                    space1,
                    self.int(),
                    space1,
                    ident_except_infopr,
                )),
                |(is_left, _, d, _, identifier)| {
                    let pred = d as usize;
                    self.push_infix(is_left, pred, identifier.clone());
                    Exp::RegInfix(InfOpr {
                        is_left: is_left,
                        pred: pred,
                        opr: identifier,
                    })
                },
            )(s)
        }
    }
}
