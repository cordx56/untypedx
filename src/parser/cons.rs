use super::Parser;
use crate::define;
use nom::{
    branch::{alt, permutation},
    bytes::complete::{escaped_transform, is_not, tag, take_while_m_n},
    character::complete::{char, digit1, line_ending, multispace0, none_of, space0, space1},
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
        |s: &str| {
            alt((
                map(self.int(), |int| Cons::Int(int)),
                map(self.real(), |real| Cons::Real(real)),
                map(self.string(), |string| Cons::String(string)),
            ))(s)
        }
    }
    pub fn int<'a>(
        &self,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, i64, VerboseError<&'a str>> + 'a {
        |s: &str| {
            map(
                tuple((opt(tag(define::MINUS_SIGN)), digit1)),
                |(minus, digit): (Option<&str>, &str)| {
                    let val: i64 = digit.parse().unwrap();
                    if minus.is_some() {
                        -1 * val
                    } else {
                        val
                    }
                },
            )(s)
        }
    }
    pub fn real<'a>(
        &self,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, f64, VerboseError<&'a str>> + 'a {
        |s: &str| double(s)
    }
    pub fn string(&self) -> impl FnMut(&str) -> IResult<&str, String, VerboseError<&str>> + '_ {
        |s: &str| alt((self.string_empty(), self.string_content()))(s)
    }
    pub fn string_content(
        &self,
    ) -> impl FnMut(&str) -> IResult<&str, String, VerboseError<&str>> + '_ {
        |s: &str| {
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
            )(s)
        }
    }
    pub fn string_empty(
        &self,
    ) -> impl FnMut(&str) -> IResult<&str, String, VerboseError<&str>> + '_ {
        |s: &str| {
            map(
                permutation((tag(define::STRING_QUOTE), tag(define::STRING_QUOTE))),
                |(_, _)| "".to_owned(),
            )(s)
        }
    }
}
