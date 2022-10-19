#[derive(Clone)]
enum Exp {
    Lambda(Lambda),
    Var(char),
    Const(i32),
}

impl Exp {
    fn replace_var(self, var: char, new_exp: Box<&Exp>) -> Box<Exp> {
        match self {
            Exp::Lambda(l) => l.replace_var(var, new_exp),
            Exp::Var(v) if v == var => Box::new((**new_exp).clone()),
            _ => Box::new(self),
        }
    }
}

#[derive(Clone)]
struct Lambda {
    arg: char,
    exp: Box<Exp>,
}

impl Lambda {
    fn replace_var(self, var: char, new_exp: Box<&Exp>) -> Box<Exp> {
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
