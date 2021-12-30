

use super::exp::Exp;

#[derive(Debug, Clone)]
pub struct Stmt(Exp);
#[derive(Debug, Clone)]
pub struct Stmts(Vec<Exp>);
