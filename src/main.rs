#[derive(Clone)]
enum Exp {
    Lambda(Lambda),
    Var(char),
    Const(i32),
    Op(Operation),
}

impl Exp {
    fn replace_var(self, var: char, new_exp: &Box<Exp>) -> Box<Exp> {
        match self {
            Exp::Lambda(l) => l.replace_var(var, new_exp),
            Exp::Var(v) if v == var => new_exp.clone(),
            Exp::Op(op) => op.replace_var(var, new_exp),
            _ => Box::new(self),
        }
    }
}

#[derive(Clone, Copy)]
enum Operator {
    Add,
    Sub,
    Mul,
}

#[derive(Clone)]
struct Operation {
    op: Operator,
    e1: Box<Exp>,
    e2: Box<Exp>,
}

impl Operation {
    fn replace_var(self, var: char, new_exp: &Box<Exp>) -> Box<Exp> {
        Box::new(Exp::Op(Self {
            op: self.op,
            e1: self.e1.replace_var(var, new_exp),
            e2: self.e2.replace_var(var, new_exp),
        }))
    }
}

#[derive(Clone)]
struct Lambda {
    arg: char,
    exp: Box<Exp>,
}

impl Lambda {
    fn replace_var(self, var: char, new_exp: &Box<Exp>) -> Box<Exp> {
        if var == self.arg {
            return Box::new(Exp::Lambda(self));
        }

        return Box::new(Exp::Lambda(Self {
            arg: self.arg,
            exp: self.exp.replace_var(var, new_exp),
        }));
    }
}

impl Lambda {
    fn new(arg: char, exp: Box<Exp>) -> Self {
        Self { arg, exp }
    }
}

fn main() {}
