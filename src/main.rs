use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Exp {
    Lambda(Lambda),
    Var(Identifier),
    Num(i32),
    Not(Box<Exp>),
    Op(Box<Exp>, Operator, Box<Exp>),
}

#[derive(Debug, Clone)]
struct Lambda {
    arg: Identifier,
    exp: Box<Exp>,
}

#[derive(Debug, Clone, Copy)]
enum Operator {
    Add,
    Mul,
    App,
    Less,
}

struct Ass {
    identifier: Identifier,
    value: Box<Exp>,
}

type Prog = Vec<Ass>;

type Identifier = String;
type State = HashMap<Identifier, Box<Exp>>;

macro_rules! parse {
    // programm
    ($((def $iden:ident $exp:tt))+) => {
        {
            let mut temp_vec:Prog = Vec::new();
            $(
                temp_vec.push(Ass {
                    identifier: format!("{}",stringify!($iden)),
                    value: parse!($exp),
                });
            )*
            temp_vec
        }
    };
    // constants
    (true) => {
        parse!(1)
    };
    (false) => {
        parse!(0)
    };
    // var
    ($var:ident) => {
        {
            let s = format!("{}",stringify!($var));
            Box::new(Exp::Var(s))
        }
    };
    //num
    ($int:literal) => {
        Box::new(Exp::Num($int))
    };
    // minus
    ((-$e:tt)) => {
        Box::new(Exp::Op(Box::new(Exp::Num(-1)),Operator::Mul,parse!($e)))
    };
    // 1 input opp
    ((! $e2:tt)) => {
        Box::new(Exp::Not(parse!($e2)))
    };
    // 2 input opp
    (($e1:tt + $e2:tt)) => {
        Box::new(Exp::Op(parse!($e1), Operator::Add, parse!($e2)))
    };
    (($e1:tt * $e2:tt)) => {
        Box::new(Exp::Op(parse!($e1), Operator::Mul, parse!($e2)))
    };
    (($e1:tt < $e2:tt)) => {
        Box::new(Exp::Op(parse!($e1), Operator::Less, parse!($e2)))
    };
    // lambda
    (($arg:ident -> $exp:tt)) => {
        Box::new(Exp::Lambda(Lambda {
            arg: format!("{}",stringify!($arg)),
            exp: parse!($exp),
        }))
    };
    // function application
    (($func:tt $($arg:tt)+)) => {
        {
            let mut cur = parse!($func);
            $(
                cur = Box::new(Exp::Op(cur, Operator::App, parse!($arg)));
            )*
            cur
        }
    };
}

macro_rules! make_state {
    ($((def $name:ident $exp:tt))*) => {
        {
            let mut temp:State = HashMap::new();
            $(
                temp.insert(
                    format!("{}",stringify!($name)),
                    Box::new(parse!($exp).eval(&temp))
                );
            )*
            temp
        }
    };
}

impl Exp {
    fn eval(self, state: &State) -> Exp {
        match self {
            Exp::Not(e) => match e.eval(state) {
                Exp::Num(n) => Exp::Num((n == 0) as i32),
                e => Exp::Not(Box::new(e)),
            },
            Exp::Var(v) => {
                if state.contains_key(&v) {
                    *state.get(&v).unwrap().clone()
                } else {
                    Exp::Var(v)
                }
            }
            Exp::Op(e1, op, e2) => op.eval(state, *e1, *e2),
            e => e,
        }
    }
    fn to_string(&self) -> String {
        match self {
            Exp::Lambda(l) => l.to_string(),
            Exp::Not(e1) => format!("!{}", e1.to_string()),
            Exp::Var(v) => format!("{v}"),
            Exp::Num(n) => format!("{n}"),
            Exp::Op(e1, Operator::App, e2) => {
                format!("({} {})", e1.to_string(), e2.to_string())
            }
            Exp::Op(e1, op, e2) => {
                format!("({} {} {})", e1.to_string(), op.get_char(), e2.to_string())
            }
        }
    }

    fn replace_var(self, var: &Identifier, new_exp: &Exp) -> Box<Exp> {
        match self {
            Exp::Lambda(l) => l.replace_var(var, new_exp),
            Exp::Var(v) if v == *var => Box::new(new_exp.clone()),
            Exp::Var(v) => Box::new(Exp::Var(v)),
            Exp::Op(e1, op, e2) => Box::new(Exp::Op(
                e1.replace_var(var, new_exp),
                op,
                e2.replace_var(var, new_exp),
            )),
            Exp::Not(e) => Box::new(Exp::Not(e.replace_var(var, new_exp))),
            Exp::Num(n) => Box::new(Exp::Num(n)),
        }
    }
}

impl Operator {
    fn get_char(&self) -> char {
        match self {
            Operator::Add => '+',
            Operator::Mul => '*',
            Operator::App => unreachable!("Operator::App has no associated character"),
            Operator::Less => '<',
        }
    }

    fn eval(self, state: &State, e1: Exp, e2: Exp) -> Exp {
        match self {
            Operator::Add => match e1.eval(state) {
                Exp::Num(0) => e2.eval(state),
                Exp::Num(n1) => match e2.eval(state) {
                    Exp::Num(n2) => Exp::Num(n1 + n2),
                    e2 => Exp::Op(Box::new(Exp::Num(n1)), Operator::Add, Box::new(e2)),
                },
                e1 => match e2.eval(state) {
                    Exp::Num(0) => e1,
                    e2 => Exp::Op(Box::new(e1), Operator::Add, Box::new(e2)),
                },
            },
            Operator::App => match e1.eval(state) {
                Exp::Lambda(l) => l.apply(&Box::new(e2)).eval(state),
                Exp::Var(v) => {
                    println!("Exiting with error:\n{}", v);
                    std::process::exit(1);
                }
                e => Exp::Op(Box::new(e), Operator::Add, Box::new(e2)),
            },
            Operator::Mul => match e1.eval(state) {
                Exp::Num(0) => Exp::Num(0),
                Exp::Num(1) => e2.eval(state),
                Exp::Num(n1) => match e2.eval(state) {
                    Exp::Num(n2) => Exp::Num(n1 * n2),
                    e2 => Exp::Op(Box::new(Exp::Num(n1)), Operator::Mul, Box::new(e2)),
                },
                e1 => match e2.eval(state) {
                    Exp::Num(0) => Exp::Num(0),
                    Exp::Num(1) => e1,
                    e2 => Exp::Op(Box::new(e1), Operator::Mul, Box::new(e2)),
                },
            },
            Operator::Less => match (e1.eval(state), e2.eval(state)) {
                (Exp::Num(n1), Exp::Num(n2)) => Exp::Num((n1 < n2) as i32),
                (e1, e2) => Exp::Op(Box::new(e1), Operator::Less, Box::new(e2)),
            },
        }
    }
}

impl Lambda {
    fn to_string(&self) -> String {
        format!("({} -> {})", self.arg, self.exp.to_string())
    }

    fn apply(&self, input: &Box<Exp>) -> Box<Exp> {
        self.exp.clone().replace_var(&self.arg, input)
    }

    fn replace_var(self, var: &Identifier, new_exp: &Exp) -> Box<Exp> {
        if *var == self.arg {
            return Box::new(Exp::Lambda(self));
        }

        return Box::new(Exp::Lambda(Self {
            arg: self.arg,
            exp: self.exp.replace_var(var, new_exp),
        }));
    }
}

fn run_prog(mut prog: Prog, state: &mut State) {
    while prog.len() > 0 {
        let ass = prog.remove(0);
        let tmp = Box::new(ass.value.eval(&state));
        state.insert(ass.identifier, tmp);
    }
}

fn main() {
    // built in functions written in the lang itself
    // `not` aka `!` was once here too, but to increaase performace it is now in the interpreter
    let mut state = make_state!(
        (def if (_condition -> (_exp_a -> (_exp_b -> (((!(! _condition)) * _exp_a) + ((! _condition) * _exp_b))))))
        (def create_list (_cur_val -> (_next_node -> (_index -> (if _index (_next_node (_index + (-1))) _cur_val)))))
        (def func_concat (_f1 -> (_f2 -> (_x -> (_f1 (_f2 _x))))))
        (def map func_concat) 
    );

    let if_prog: Prog = parse!(
        (def list (create_list 0 (create_list 1 (create_list 2 OutOfBounds_Error))))
        (def double (_x -> (_x * 2)))
        (def plus_two (_x -> (_x + 2)))
        (def combined (func_concat double plus_two))
        (def mapped_list (map combined list))
        (def res (mapped_list 1))
    );

    run_prog(if_prog, &mut state);
    println!("value of res: {}", parse!(res).eval(&state).to_string());
}
