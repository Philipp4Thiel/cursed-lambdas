trait Exp {
    fn replace_var(self, var: char, new_exp: Box<&dyn Exp>) -> Box<dyn Exp>;
}

struct Lambda {
    arg: char,
    exp: Box<dyn Exp>,
}

impl Exp for Lambda {
    fn replace_var(self, var: char, new_exp: Box<&dyn Exp>) -> Box<dyn Exp> {
        if var == self.arg {
            return Box::new(self);
        }

        return Box::new(Self {
            arg: self.arg,
            exp: self.exp.replace_var(var, new_exp),
        });
    }
}

impl Lambda {
    fn new(arg: char, exp: Box<dyn Exp>) -> Self {
        Self { arg, exp }
    }
}

fn main() {}
