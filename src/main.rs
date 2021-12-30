pub mod parser;
pub mod define;

fn main() {
    let p = parser::Parser::new();
    let exp = p.exp()("test + 1");
    println!("{:?}", exp);
}
