use super::Parser;
use crate::define;
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
use std::char::{decode_utf16, REPLACEMENT_CHARACTER};

#[derive(Debug, Clone)]
pub enum Cons {
    Int(i64),
    Real(f64),
    String(String),
}

impl Parser {
    pub fn cons(&self) -> impl FnMut(&str) -> IResult<&str, Cons, VerboseError<&str>> + '_ {
        |s: &str| alt((self.real(), self.string()))(s)
    }
    pub fn real(&self) -> impl FnMut(&str) -> IResult<&str, Cons, VerboseError<&str>> + '_ {
        |s: &str| map(double, |number: f64| -> Cons { Cons::Real(number) })(s)
    }
    pub fn string(&self) -> impl FnMut(&str) -> IResult<&str, Cons, VerboseError<&str>> + '_ {
        |s: &str| alt((self.string_empty(), self.string_content()))(s)
    }
    pub fn string_content(
        &self,
    ) -> impl FnMut(&str) -> IResult<&str, Cons, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                delimited(
                    tag(define::STRING_QUOTE),
                    escaped_transform(
                        none_of("\"\\"),
                        '\\',
                        alt((
                            value('\\', char('\\')),
                            value('\"', char('\"')),
                            value('\'', char('\'')),
                            value('\r', char('r')),
                            value('\n', char('n')),
                            value('\t', char('t')),
                            map(
                                permutation((
                                    char('u'),
                                    take_while_m_n(4, 4, |c: char| c.is_ascii_hexdigit()),
                                )),
                                |(_, code): (char, &str)| -> char {
                                    decode_utf16(vec![u16::from_str_radix(code, 16).unwrap()])
                                        .nth(0)
                                        .unwrap()
                                        .unwrap_or(REPLACEMENT_CHARACTER)
                                },
                            ),
                        )),
                    ),
                    tag(define::STRING_QUOTE),
                ),
                |string: String| Cons::String(string),
            )(s)
        }
    }
    pub fn string_empty(&self) -> impl FnMut(&str) -> IResult<&str, Cons, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                permutation((tag(define::STRING_QUOTE), tag(define::STRING_QUOTE))),
                |(_, _)| Cons::String("".to_owned()),
            )(s)
        }
    }
}
