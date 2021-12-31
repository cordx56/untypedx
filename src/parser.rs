pub mod cons;
pub mod exp;
pub mod ident;
pub mod infix;
pub mod stmt;

#[derive(Debug, Clone)]
pub struct InfOpr {
    is_left: bool,
    pred: usize,
    opr: String,
}

#[derive(Clone)]
pub struct Parser {
    pub infix: Vec<InfOpr>,
}

impl Parser {
    pub fn new() -> Self {
        let mut parser = Parser { infix: Vec::new() };

        parser.push_infix(true, 2, "=".to_owned());
        parser.push_infix(true, 4, "<>".to_owned());
        parser.push_infix(true, 4, "!=".to_owned());
        parser.push_infix(true, 4, "<".to_owned());
        parser.push_infix(true, 4, ">".to_owned());
        parser.push_infix(true, 4, "<=".to_owned());
        parser.push_infix(true, 4, ">=".to_owned());
        parser.push_infix(true, 6, "+".to_owned());
        parser.push_infix(true, 6, "-".to_owned());
        parser.push_infix(true, 7, "*".to_owned());
        parser.push_infix(true, 7, "/".to_owned());
        parser.push_infix(true, 7, "%".to_owned());
        parser
    }

    pub fn push_infix(&mut self, is_left: bool, pred: usize, opr: String) {
        self.infix.push(InfOpr {
            is_left: is_left,
            pred: pred,
            opr: opr,
        });
        self.infix.sort_by(|a, b| b.opr.len().cmp(&a.opr.len()));
    }
}

// Rebuild AST using shunting-yard algorithm
fn rebuild_tree<Elem, Opr>(
    pred_func: fn(&Opr) -> i8,
    is_left_assoc_func: fn(&Opr) -> bool,
    elem_func: fn(Opr, Elem, Elem) -> Elem,
    left: Elem,
    tail: Vec<(Opr, Elem)>,
) -> Elem {
    let mut operands = Vec::new();
    let mut operators: Vec<(Opr, i8)> = Vec::new();
    operands.push(left);
    for (opr, elem) in tail {
        let p = pred_func(&opr);
        let p_left = is_left_assoc_func(&opr);
        while !operators.is_empty()
            && ((p_left && p <= operators.last().unwrap().1) || p < operators.last().unwrap().1)
        {
            let (opr, _) = operators.pop().unwrap();
            let r2 = operands.pop().unwrap();
            let r1 = operands.pop().unwrap();
            operands.push(elem_func(opr, r1, r2));
        }
        operators.push((opr, p));
        operands.push(elem);
    }
    while !operators.is_empty() {
        let (opr, _) = operators.pop().unwrap();
        let r2 = operands.pop().unwrap();
        let r1 = operands.pop().unwrap();
        operands.push(elem_func(opr, r1, r2));
    }
    let ast = operands.pop().unwrap();
    ast
}
