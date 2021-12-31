pub mod define;
pub mod parser;

fn main() {
    let p = parser::Parser::new();
    println!("{:?}", p.exp()("test + 1 * 2"));
    println!("{:?}", p.exp()("test * 1 - 2"));
    println!("{:?}", p.exp()("test 2"));
    println!("{:?}", p.exp()("let x = 1"));
}
