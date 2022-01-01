pub mod define;
pub mod parser;

use clap::Parser;
use std::fs;
use std::io;
use std::io::{Read, Write};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[clap(about, version, author)]
struct Args {
    file: Option<String>,
}

fn main() {
    let p = parser::Parser::new();
    //println!("{:?}", p.stmts()("test + 1 * 2"));
    //println!("{:?}", p.stmts()("test * 1 - 2"));
    //println!("{:?}", p.stmts()("test 2"));
    //println!("{:?}", p.stmts()("let x = fn x y => x + y"));
    let args = Args::parse();

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
    }
    println!("{}", buffer);
    let result = p.stmts()(&buffer);
    println!("{:?}", result);
    if let Err(nom::Err::Error(e)) = result {
        let verbose_error = nom::error::convert_error(&buffer as &str, e);
        println!("{}", verbose_error);
    }
}
