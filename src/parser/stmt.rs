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

use super::exp::Exp;
use super::Parser;
use crate::define;

#[derive(Debug, Clone)]
pub struct Stmt(Exp);
#[derive(Debug, Clone)]
pub struct Stmts(Vec<Exp>);

impl Parser {
    pub fn stmt(&self) -> impl FnMut(&str) -> IResult<&str, Stmt, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                tuple((self.exp(), space0, tag(define::STATEMENT_END))),
                |(exp, _, _)| Stmt(exp),
            )(s)
        }
    }

    pub fn stmts(&self) -> impl FnMut(&str) -> IResult<&str, Stmts, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                tuple((
                    many0(map(tuple((multispace0, self.stmt())), |(_, stmt)| stmt.0)),
                    multispace0,
                    opt(self.exp()),
                )),
                |(stmts, _, exp_opt)| {
                    let mut vec = stmts;
                    if let Some(exp) = exp_opt {
                        vec.push(exp);
                    }
                    Stmts(vec)
                },
            )(s)
        }
    }
}
