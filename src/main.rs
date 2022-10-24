use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Exp {
    Lambda(Lambda),
    Var(Identifier),
    Num(i32),
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
    // op
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
    ($(($name:ident $exp:tt))+) => {
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
            Exp::Op(e1, op, e2) => op.eval(state, *e1, *e2),
            Exp::Lambda(l) => Exp::Lambda(l),
            Exp::Var(v) => {
                if state.contains_key(&v) {
                    state.get(&v).unwrap().clone().eval(state)
                } else {
                    Exp::Var(v)
                }
            }
            Exp::Num(n) => Exp::Num(n),
        }
    }
    fn to_string(&self) -> String {
        match self {
            Exp::Lambda(l) => l.to_string(),
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
            Operator::App => ' ',
            Operator::Less => '<',
        }
    }

    fn eval(self, state: &State, e1: Exp, e2: Exp) -> Exp {
        match self {
            Operator::Mul => match e1.eval(state) {
                Exp::Num(0) => Exp::Num(0),
                Exp::Num(1) => e2.eval(state),
                e1 => match (e1, e2.eval(state)) {
                    (Exp::Num(c1), Exp::Num(c2)) => Exp::Num(c1 * c2),
                    (_, Exp::Num(0)) => Exp::Num(0),
                    (e1, Exp::Num(1)) => e1,
                    (e1, e2) => Exp::Op(Box::new(e1), Operator::Mul, Box::new(e2)),
                },
            },

            Operator::Add => match (e1.eval(state), e2.eval(state)) {
                (Exp::Num(c1), Exp::Num(c2)) => Exp::Num(c1 + c2),
                (e1, Exp::Num(0)) => e1,
                (Exp::Num(0), e2) => e2,
                (e1, e2) => Exp::Op(Box::new(e1), Operator::Add, Box::new(e2)),
            },

            Operator::Less => match (e1.eval(state), e2.eval(state)) {
                (Exp::Num(c1), Exp::Num(c2)) => Exp::Num((c1 < c2) as i32),
                (e1, e2) => Exp::Op(Box::new(e1), Operator::Less, Box::new(e2)),
            },

            Operator::App => match e1 {
                Exp::Var(v) => Exp::Op(
                    Box::new(Exp::Var(v).eval(state)),
                    Operator::App,
                    Box::new(e2),
                )
                .eval(state)
                .eval(state),
                Exp::Lambda(l) => l.apply(&Box::new(e2)).eval(state).eval(state),
                Exp::Num(_) => panic!("tried to apply int"),
                e1 => Exp::Op(Box::new(e1.eval(state)), Operator::App, Box::new(e2)),
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

impl Ass {
    fn to_string(&self) -> String {
        format!("(def {} {})", self.identifier, self.value.to_string())
    }
}

fn run_prog(mut prog: Prog, mut state: State) -> Exp {
    println!("program:");
    for a in &prog {
        println!("{}", a.to_string());
    }
    println!();

    while prog.len() > 1 {
        let ass = prog.remove(0);
        // println!(
        //     "evaluating assignment: {} = {}\n",
        //     ass.identifier,
        //     ass.value.to_string()
        // );
        let tmp = Box::new(ass.value.eval(&state));
        // println!(
        //     "\nresulting assignment: {} = {}\n",
        //     ass.identifier,
        //     tmp.to_string()
        // );
        state.insert(ass.identifier, tmp);
    }
    let ass = prog.remove(0);
    // println!("evaluating last exp:\n{}", ass.value.to_string());
    ass.value.eval(&state)
}

fn main() {
    // built in functions written in the lang itself
    let state = make_state!(
        (if (c -> (a -> (b -> ((((x -> ((x < (-x)) + ((-x) < x))) c) * a) + (((x -> ((-((x -> ((x < (-x)) + ((-x) < x))) x)) + 1)) c) * b))))))
    );

    let if_prog: Prog = parse!(
        (def rec_times_2 (x -> (if (x < 1) 0 (2 + (rec_times_2 (x + (-1)))))))
        (def res (rec_times_2 410))
        // (def b (a -> (3 * a)))
        // (def c (b (b (b (b 3)))))
    );

    let res = run_prog(if_prog, state);
    println!("value of last exp:");
    println!("{}", res.to_string());
}
