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
    Function(Box<Ty>, Box<Ty>),
}

impl Parser {
    //pub fn ty<'a>(&self) -> impl FnMut(&'a str) -> IResult<&'a str, Exp, VerboseError<&'a str>> {
    //}
}
