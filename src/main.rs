pub mod define;
pub mod infer;
pub mod interpreter;
pub mod parser;

use clap::{Parser, Subcommand};
use infer::Inferer;
use nom::combinator::all_consuming;
use std::collections::HashSet;
use std::fs;
use std::io;
use std::io::{Read, Write};

#[derive(Parser)]
#[clap(about, version, author)]
struct Cli {
    #[clap(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Run code
    Run { file: Option<String> },
    /// Perform type inference
    Infer { file: Option<String> },
}

fn main() {
    let cli = Cli::parse();

    let mut p = parser::Parser::new();
    let mut i = interpreter::Interpreter::new();
    let mut inferer = Inferer::new();
    //println!("{:?}", p.stmts()("test + 1 * 2"));
    //println!("{:?}", p.stmts()("test * 1 - 2"));
    //println!("{:?}", p.stmts()("test 2"));
    //println!("{:?}", p.stmts()("let x = fn x y => x + y"));

    let mut buffer = String::new();
    match &cli.command {
        Some(Commands::Run { file }) => {
            if let Some(path) = file {
                match file_read(path) {
                    Ok(buffer) => {
                        let presult = p.stmts()(&buffer);
                        println!("{:?}", presult);
                    }
                    Err(e) => eprintln!("{}", e),
                }
            } else {
                println!("UnTypedX Interpreter Ver:{}", i.version);
                interactive_mode(move |buffer: &str| match all_consuming(p.exp())(buffer) {
                    Ok(e) => match inferer.analyze(&e.1, &HashSet::new()) {
                        Ok(result) => {
                            println!("{}", inferer.display_type(result));
                            false
                        }
                        Err(e) => {
                            eprintln!("{}", e);
                            false
                        }
                    },
                    Err(_) => true,
                });
            }
        }
        Some(Commands::Infer { file }) => {
            if let Some(path) = file {
                match file_read(path) {
                    Ok(buffer) => match all_consuming(p.stmts())(&buffer) {
                        Ok(pr) => match inferer.infer(&pr.1) {
                            Ok(tid) => println!(
                                "Type inference success!\nReturn type: {}",
                                inferer.display_type(tid)
                            ),
                            Err(e) => eprintln!("Error found!\n{}", e),
                        },
                        Err(_) => eprintln!("Parse error!"),
                    },
                    Err(e) => eprintln!("{}", e),
                }
            } else {
                println!("UnTypedX Interpreter Ver:{}", i.version);
                interactive_mode(move |buffer: &str| match all_consuming(p.exp())(buffer) {
                    Ok(e) => match inferer.analyze(&e.1, &HashSet::new()) {
                        Ok(result) => {
                            println!("{}", inferer.display_type(result));
                            false
                        }
                        Err(e) => {
                            eprintln!("{}", e);
                            false
                        }
                    },
                    Err(_) => true,
                })
            }
        }
        None => {
            println!("UnTypedX Interpreter Ver:{}", i.version);
            interactive_mode(move |buffer: &str| match all_consuming(p.exp())(buffer) {
                Ok(e) => match inferer.analyze(&e.1, &HashSet::new()) {
                    Ok(result) => {
                        println!("{}", inferer.display_type(result));
                        false
                    }
                    Err(e) => {
                        eprintln!("{}", e);
                        false
                    }
                },
                Err(_) => true,
            });
        }
    }
}

fn file_read(path: &str) -> Result<String, String> {
    if path == "-" {
        let mut buffer = String::new();
        let stdin = io::stdin();
        let mut handle = stdin.lock();
        handle.read_to_string(&mut buffer).ok();
        Ok(buffer)
    } else {
        match fs::read_to_string(path) {
            Ok(s) => Ok(s),
            Err(e) => return Err(e.to_string()),
        }
    }
}

fn interactive_mode(mut func: impl FnMut(&str) -> bool) {
    let mut buffer = String::new();
    loop {
        if 0 < buffer.len() {
            print!(". ");
        } else {
            print!("> ");
        }
        std::io::stdout().flush().ok();
        std::io::stdin().read_line(&mut buffer).ok();
        if buffer.trim_end() == ":q" {
            break;
        }
        if func(&buffer.trim()) {
            continue;
        }
        buffer = String::new();
    }
}
