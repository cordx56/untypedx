use nom::{
    branch::{alt, permutation},
    bytes::complete::{escaped_transform, is_not, tag, take_while_m_n},
    character::complete::{char, line_ending, multispace0, none_of, space0, space1},
    combinator::{all_consuming, map, opt, value},
    error::VerboseError,
    multi::{many0, many1},
    number::complete::double,
    sequence::{delimited, tuple},
    IResult,
};

use crate::define;
use super::Parser;

impl Parser {
    pub fn ident(&self) -> impl FnMut(&str) -> IResult<&str, String, VerboseError<&str>> + '_ {
        |s: &str| {
            map(is_not(define::PARSER_NOT_IDENTIFIER), |identifier: &str| {
                identifier.to_owned()
            })(s)
        }
    }
}
