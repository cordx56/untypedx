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
use super::{InfOpr, Parser};
use crate::define;

#[derive(Debug, Clone)]
pub enum Stmt {
    Exp(Exp),

    RegInfix(InfOpr, Exp),
}
#[derive(Debug, Clone)]
pub struct Stmts(Vec<Stmt>);

impl Parser {
    pub fn stmt(&mut self) -> impl FnMut(&str) -> IResult<&str, Stmt, VerboseError<&str>> + '_ {
        let clone = self.clone();
        move |s: &str| {
            alt((
                map(
                    tuple((clone.exp(), space0, tag(define::STATEMENT_END))),
                    |(exp, _, _)| Stmt::Exp(exp),
                ),
                self.reginfexp(),
            ))(s)
        }
    }

    pub fn reginfexp(
        &mut self,
    ) -> impl FnMut(&str) -> IResult<&str, Stmt, VerboseError<&str>> + '_ {
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
                    space0,
                    tag(define::EQUAL),
                    space0,
                    clone.fnexp(),
                )),
                |(is_left, _, d, _, identifier, _, _, _, exp)| {
                    let pred = d as usize;
                    self.push_infix(is_left, pred, identifier.clone());
                    Stmt::RegInfix(
                        InfOpr {
                            is_left: is_left,
                            pred: pred,
                            opr: identifier,
                        },
                        exp,
                    )
                },
            )(s)
        }
    }

    pub fn stmts(&mut self) -> impl FnMut(&str) -> IResult<&str, Stmts, VerboseError<&str>> + '_ {
        |s: &str| {
            let (s, stmts) = many0(map(tuple((multispace0, self.stmt())), |(_, stmt)| stmt))(s)?;
            let (s, _) = multispace0(s)?;
            let (s, exp_opt) = opt(self.exp())(s)?;
            let mut result = stmts;
            if let Some(exp) = exp_opt {
                result.push(Stmt::Exp(exp));
            }
            Ok((s, Stmts(result)))
        }
    }
}
