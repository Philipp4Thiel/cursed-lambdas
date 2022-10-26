use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Exp {
    Lambda(Lambda),
    Var(Identifier),
    Num(i32),
    Not(Box<Exp>),
    Op(Box<Exp>, Operator, Box<Exp>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Lambda {
    arg: Identifier,
    exp: Box<Exp>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Operator {
    Eq,
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
    // num
    ($int:literal) => {
        Box::new(Exp::Num($int))
    };
    // minus
    ((-$e:tt)) => {
        Box::new(Exp::Op(Box::new(Exp::Num(-1)),Operator::Mul,parse!($e)))
    };
    // 1 input opp
    ((!$e2:tt)) => {
        Box::new(Exp::Not(parse!($e2)))
    };
    // 2 input opp
    (($e1:tt + $e2:tt)) => {
        Box::new(Exp::Op(parse!($e1), Operator::Add, parse!($e2)))
    };
    (($e1:tt - $e2:tt)) => {
        Box::new(Exp::Op(parse!($e1), Operator::Add, parse!((-$e2))))
    };
    (($e1:tt * $e2:tt)) => {
        Box::new(Exp::Op(parse!($e1), Operator::Mul, parse!($e2)))
    };
    (($e1:tt < $e2:tt)) => {
        Box::new(Exp::Op(parse!($e1), Operator::Less, parse!($e2)))
    };
    (($e1:tt = $e2:tt)) => {
        Box::new(Exp::Op(parse!($e1), Operator::Eq, parse!($e2)))
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
            Operator::Eq => '=',
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
            Operator::App => match e1.clone().eval(state) {
                Exp::Lambda(l) => l.apply(&Box::new(e2)).eval(state),
                Exp::Var(_) | Exp::Num(_) => {
                    println!("Exiting with error:\n{}", e1.to_string());
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
            Operator::Eq => Exp::Num((e1.eval(state) == e2.eval(state)) as i32),
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
        (def list_add (_new_val -> (_old_list -> (_in -> (if (_in = HEAD) _new_val (if (_in = TAIL) _old_list INVALID_LIST_COMMAND))))))
        (def list_print (_list -> (if (_list = NULL) NULL ((_list HEAD) + (list_print (_list TAIL))))))
        (def list_map (_f -> (_list -> (list_add (_f (_list HEAD)) (if ((_list TAIL) = NULL) NULL (list_map _f (_list TAIL)))))))
    );

    let if_prog: Prog = parse!(
        (def list (list_add 0 (list_add 1 (list_add 2 (list_add 3 NULL)))))
        (def double (x -> (2 * x)))
        (def mapped (list_map double list))
        (def res (list_print mapped))
    );

    run_prog(if_prog, &mut state);
    println!("value of res: {}", parse!(res).eval(&state).to_string());
}
