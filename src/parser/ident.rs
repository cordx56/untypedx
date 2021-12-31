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

use super::Parser;
use crate::define;

impl Parser {
    pub fn ident_except_infopr(
        &self,
    ) -> impl FnMut(&str) -> IResult<&str, String, VerboseError<&str>> + '_ {
        |s: &str| match is_not(define::PARSER_NOT_IDENTIFIER)(s) {
            Ok(res) => {
                for v in define::IDENTIFIER_NOT_ALLOWED {
                    if &res.1 == v {
                        return Err(Err::Error(VerboseError::from_error_kind(
                            s,
                            nom::error::ErrorKind::Fail,
                        )));
                    }
                }
                for v in &self.infix {
                    if res.1 == v.opr {
                        return Err(Err::Error(VerboseError::from_error_kind(
                            s,
                            nom::error::ErrorKind::Fail,
                        )));
                    }
                }
                Ok((res.0, res.1.to_owned()))
            }
            Err(e) => Err(e),
        }
    }
}
