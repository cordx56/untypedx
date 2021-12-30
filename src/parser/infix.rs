use nom::{
    branch::{alt, permutation},
    bytes::complete::{escaped_transform, is_not, tag, take_while_m_n},
    character::complete::{char, line_ending, multispace0, none_of, space0, space1},
    combinator::{all_consuming, map, opt, value},
    error::{ParseError, VerboseError},
    multi::{many0, many1},
    number::complete::double,
    sequence::{delimited, tuple},
    Err, IResult,
};

use super::InfOpr;
use super::Parser;
use super::exp::Exp;

impl Parser {
    pub fn infopr(&self) -> impl FnMut(&str) -> IResult<&str, InfOpr, VerboseError<&str>> + '_ {
        |s: &str| -> IResult<&str, InfOpr, VerboseError<&str>> {
            for v in &self.infix {
                let infstr: &str = &v.opr;
                if let Ok(res) = tag::<&str, &str, VerboseError<&str>>(infstr)(s) {
                    return Ok((res.0, v.clone()));
                }
            }
            return Err(Err::Error(VerboseError::from_error_kind(
                s,
                nom::error::ErrorKind::Fail,
            )));
        }
    }

    // Rebuild AST using shunting-yard algorithm
    pub fn rebuild_tree(
        &self,
        left: &Exp,
        tail: &[(InfOpr, Exp)],
    ) -> Exp {
        let mut operands = Vec::new();
        let mut operators: Vec<(InfOpr, usize)> = Vec::new();
        operands.push(left.clone());
        for (opr, elem) in tail {
            let p = opr.pred;
            let p_left = opr.is_left;
            while !operators.is_empty()
                && ((p_left && p <= operators.last().unwrap().1) || p < operators.last().unwrap().1)
            {
                let (opr, _) = operators.pop().unwrap();
                let r2 = operands.pop().unwrap();
                let r1 = operands.pop().unwrap();
                operands.push(Exp::InfExp(Box::new(r1), opr, Box::new(r2)));
            }
            operators.push((opr.clone(), p));
            operands.push(elem.clone());
        }
        while !operators.is_empty() {
            let (opr, _) = operators.pop().unwrap();
            let r2 = operands.pop().unwrap();
            let r1 = operands.pop().unwrap();
            operands.push(Exp::InfExp(Box::new(r1), opr, Box::new(r2)));
        }
        let ast = operands.pop().unwrap();
        ast
    }
}
