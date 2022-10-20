#[derive(Debug, Clone)]
enum Exp {
    Lambda(Lambda),
    Var(char),
    Num(i32),
    Op(Box<Exp>, Operator, Box<Exp>),
}

#[derive(Debug, Clone)]
struct Lambda {
    arg: char,
    exp: Box<Exp>,
}

#[derive(Debug, Clone, Copy)]
enum Operator {
    Add,
    Sub,
    Mul,
}

macro_rules! parse {
    // variable
    ((var $var:literal)) => {
        Box::new(Exp::Var($var))
    };
    // int
    ($int:literal) => {
        Box::new(Exp::Num($int))
    };
    // op
    (($e1:tt + $e2:tt)) => {
        Box::new(Exp::Op(parse!($e1), Operator::Add, parse!($e2)))
    };
    (($e1:tt - $e2:tt)) => {
        Box::new(Exp::Op(parse!($e1), Operator::Sub, parse!($e2)))
    };
    (($e1:tt * $e2:tt)) => {
        Box::new(Exp::Op(parse!($e1), Operator::Mul, parse!($e2)))
    };
    // lambda
    (($arg:literal -> $exp:tt)) => {
        Box::new(Exp::Lambda(Lambda {
            arg: $arg,
            exp: parse!($exp),
        }))
    };
}

impl Exp {
    fn apply(&self, input: &Box<Exp>) -> Box<Exp> {
        match self {
            Exp::Lambda(l) => l.apply(input),
            _ => panic!("applied something that can't be applied"),
        }
    }
    fn to_string(&self) -> String {
        match self {
            Exp::Lambda(l) => l.to_string(),
            Exp::Var(v) => format!("(var {v})"),
            Exp::Num(n) => format!("{n}"),
            Exp::Op(e1, op, e2) => {
                format!("({} {} {})", e1.to_string(), op.get_char(), e2.to_string())
            }
        }
    }

    fn replace_var(self, var: char, new_exp: &Box<Exp>) -> Box<Exp> {
        match self {
            Exp::Lambda(l) => l.replace_var(var, new_exp),
            Exp::Var(v) if v == var => new_exp.clone(),
            Exp::Op(e1, op, e2) => Box::new(Exp::Op(
                e1.replace_var(var, new_exp),
                op,
                e2.replace_var(var, new_exp),
            )),
            _ => Box::new(self),
        }
    }
}

impl Operator {
    fn get_char(&self) -> char {
        match self {
            Operator::Add => '+',
            Operator::Mul => '*',
            Operator::Sub => '-',
        }
    }
}

impl Lambda {
    fn to_string(&self) -> String {
        format!("({} -> {})", self.arg, self.exp.to_string())
    }

    fn apply(&self, input: &Box<Exp>) -> Box<Exp> {
        self.exp.clone().replace_var(self.arg, input)
    }

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

fn main() {
    let e_vec: Vec<Box<Exp>> = vec![
        parse!(1),
        parse!(
            (var 'b')
        ),
        parse!(
            ('a' -> 1)
        ),
        parse!(
            ('a' -> (var 'a'))
        ),
        parse!((1 + 1)),
        parse!(((1 + 1) + 2)),
        parse!(
            ((var 'a') + (var 'b'))
        ),
        parse!(
            (('a' -> ((var 'b') + (var 'a'))) - (1 * ('b' -> 2)))
        ),
    ];

    for e in e_vec {
        println!("{}", e.to_string());
    }
}
