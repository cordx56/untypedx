pub mod define;
pub mod interpreter;
pub mod infer;
pub mod parser;

use clap::Parser;
use infer::{Inferer, TypeInference};
use nom::combinator::all_consuming;
use std::fs;
use std::io;
use std::io::{Read, Write};

#[derive(Parser, Debug)]
#[clap(about, version, author)]
struct Args {
    file: Option<String>,
}

fn main() {
    let args = Args::parse();

    let mut p = parser::Parser::new();
    let mut i = interpreter::Interpreter::new();
    let mut inferer = Inferer::new();
    //println!("{:?}", p.stmts()("test + 1 * 2"));
    //println!("{:?}", p.stmts()("test * 1 - 2"));
    //println!("{:?}", p.stmts()("test 2"));
    //println!("{:?}", p.stmts()("let x = fn x y => x + y"));

    let mut buffer = String::new();
    if let Some(path) = args.file {
        if path == "-" {
            let stdin = io::stdin();
            let mut handle = stdin.lock();
            handle.read_to_string(&mut buffer).ok();
        } else {
            match fs::read_to_string(path) {
                Ok(s) => {
                    buffer = s;
                }
                Err(e) => {
                    eprintln!("File read error");
                    eprintln!("{}", e);
                    return;
                }
            }
        }
    } else {
        println!("UnTypedX Interpreter Ver:{}", i.version);
        loop {
            if 0 < buffer.len() {
                print!(". ");
            } else {
                print!("> ");
            }
            std::io::stdout().flush().ok();
            std::io::stdin().read_line(&mut buffer).ok();
            if buffer.len() == 0 {
                break;
            }
            match all_consuming(p.exp())(&buffer.trim()) {
                Ok(e) => match inferer.analyze(&e.1) {
                    Ok(result) => {
                        println!("{}", result);
                    }
                    Err(e) => {
                        eprintln!("{}", e);
                    }
                },
                Err(_) => {
                    continue;
                }
            }
            buffer = String::new();
        }
    }
    println!("{}", buffer);
    let result = p.stmts()(&buffer);
    println!("{:?}", result);
    if let Err(nom::Err::Error(e)) = result {
        let verbose_error = nom::error::convert_error(&buffer as &str, e);
        println!("{}", verbose_error);
    }
}
