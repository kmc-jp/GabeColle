//
// TR-SECD Machine
//
use std::vec::Vec; // use as stack

use crate::lambda::*;

#[derive(Clone)]
pub struct Closure {
    ans: Answers,
    env: Environments
}

#[derive(Clone)]
pub enum CodeElem {
    Ret,
    Call,

    // lambda-term
    LTerm(crate::lambda::Term)
}

pub type Stacks = Vec<Closure>;
pub type Environments = Vec<(Variable, Closure)>;
pub type Code = Vec<CodeElem>;

pub enum Dumps {
    Halt,
    State(Stacks, Environments, Code, Box<Dumps>)
}

impl CodeElem {
    pub fn to_string(&self) -> String {
        use CodeElem::*;
        match &self {
            Ret => "ret".to_string(),
            Call => "call".to_string(),
            LTerm(t) => t.to_string()
        }
    }
}

impl Closure {
    pub fn new(ans: Answers, env: Environments) -> Closure {
        Closure {ans: ans, env: env}
    }

    pub fn to_string(&self) -> String {
        "(".to_string() + &self.ans.to_string() + ", " + &env_to_string(&self.env) + ")"
    }
}


// -----------------------------------------------------------------------------
// for debug print
// -----------------------------------------------------------------------------

pub fn stack_to_string(s: &Stacks) -> String {
    "(".to_string() + &s.into_iter().map(|l| l.to_string()).collect::<Vec<String>>().as_slice().join(" :: ") + ")"
}

pub fn env_to_string(e: &Environments) -> String {
    "(".to_string()
        + &e.into_iter().map(|(v, l)| format!("({}, {})", v, l.to_string()))
            .collect::<Vec<String>>().as_slice().join(" :: ")
        + ")"
}

pub fn code_to_string(c: &Code) -> String {
    "(".to_string() + &c.into_iter().map(|c| c.to_string()).collect::<Vec<String>>().as_slice().join(" :: ") + ")"
}

impl Dumps {
    pub fn to_string(&self) -> String {
        use Dumps::*;
        match &self {
            Halt => "halt".to_string(),
            State(s, e, c, d) => {
                "S = ".to_string() + &stack_to_string(s)
                    + ", E = " + &env_to_string(e)
                    + ", C = " + &code_to_string(c)
                    + ", D = " + &d.to_string() + "\n"
            }
        }
    }
}


// -----------------------------------------------------------------------------
// execution
// -----------------------------------------------------------------------------

fn lookup(var: &Variable, env: &Environments) -> Result<Closure, String> {
    for i in 0..env.len() {
        let (v, l) = &env[env.len() - 1 - i];
        if v == var { return Ok(l.clone()) }
    }
    Err("Variable not found: ".to_string() + &var)
}


fn pop_code(mut c: Code) -> Result<Code, String> {
    if c.len() == 0 { return Err("pop empty code".to_string()) }
    c.pop();
    Ok(c)
}

// one-step evaluation
pub fn step(dumps: Dumps) -> Result<Dumps, String> {
    use CodeElem::*;
    use Dumps::*;

    match dumps {
        State(s, env, code, dumps) => {
            match &code[..] {
                [.., LTerm(Term::Var(v))] => { // [VAR]
                    let mut ns = s;
                    ns.push(lookup(v, &env)?);
                    Ok(State(ns, env, pop_code(code)?, dumps))
                },
                [.., LTerm(Term::Const(c))] => { // [CONST]
                    let mut ns = s;
                    ns.push(Closure::new(Answers::Const(c.clone()), vec![]));
                    Ok(State(ns, env, pop_code(code)?, dumps))
                },
                [.., LTerm(Term::Abst(v, t))] => { // [ABST]
                    let mut ns = s;
                    ns.push(Closure::new(Answers::Abst(v.clone(), t.clone()), env.clone()));
                    Ok(State(ns, env, pop_code(code)?, dumps))
                },
                [Ret, LTerm(Term::App(t1, t2))] => { // [CALL]
                    Ok(State(s, env, vec![Call, LTerm((**t1).clone()), LTerm((**t2).clone())], dumps))
                },
                [.., LTerm(Term::App(t1, t2))] => { // [PUSH]
                    Ok(State(vec![],
                             env.clone(),
                             vec![Call, LTerm((**t1).clone()), LTerm((**t2).clone())],
                             Box::new(State(s, env, pop_code(code)?, dumps))))
                }
                [Ret] => { // [POP]
                    if s.len() == 0 { return Err("step error".to_string()) };
                    match *dumps {
                        State(mut ns, nenv, nc, nd) => {
                            ns.push(s[s.len() - 1].clone());
                            Ok(State(ns, nenv, nc, nd))
                        },
                        _ => Err("unexpected halt".to_string())
                    }
                },
                [Call] => { // [APPLY], [DELTA]
                    match &s[..] {
                        [.., l2, l1] => {
                            match &l1.ans {
                                Answers::Abst(v, t) => {
                                    let mut ns = s.clone();
                                    ns.pop(); ns.pop();
                                    let mut ne = l1.env.clone();
                                    ne.push(((*v).clone(), (*l2).clone()));
                                    Ok(State(ns, ne, vec![Ret, LTerm((**t).clone())], dumps))
                                },
                                Answers::Const(c1) => {
                                    match &l2.ans {
                                        Answers::Const(c2) => {
                                            let res = delta((*c1).clone(), (*c2).clone());
                                            let mut ns = s;
                                            ns.pop(); ns.pop();
                                            ns.push(Closure::new(res, vec![]));
                                            Ok(State(ns, env, vec![Ret], dumps))
                                        }
                                        _ => Err("step error".to_string())
                                    }
                                }
                            }
                        },
                        _ => Err("step error".to_string())
                    }
                }

                _ => Err("step error".to_string())
            }
        },
        _ => Err("cannot step: halt".to_string()) // HALT
    }
}


fn subst(t: &Term, val: &Values, var: &Variable) -> Term {
    match &t {
        Term::Abst(v, t) => subst2(&Answers::Abst((*v).clone(), (*t).clone()), val, var).to_term(),
        Term::Var(v) => if v == var { val.to_term() } else { Term::Var((*v).clone()) },
        Term::App(t1, t2) => Term::App(Box::new(subst(t1, val, var)), Box::new(subst(t2, val, var))),
        _ => (*t).clone()
    }
}

fn subst2(ans: &Answers, val: &Values, var: &Variable) -> Answers {
    match &ans {
        Answers::Const(c) => Answers::Const((*c).clone()),
        Answers::Abst(v, t) => {
            if v == var {
                Answers::Abst((*v).clone(), (*t).clone())
            } else {
                Answers::Abst((*v).clone(), Box::new(subst(t, val, var)))
            }
        }
    }
}

fn real(l: Closure) -> Answers {
    match &l.env[..] {
        [] => l.ans,
        [tl.., (v, l2)] => {
            real(Closure::new(subst2(&l.ans, &real((*l2).clone()).to_value(), v), tl.to_vec()))
        }
    }
}

// entry-point
pub fn run(dumps: Dumps) -> Result<Answers, String> {
    match dumps {
        Dumps::Halt => Err("Error[run]: cannot run halt.".to_string()),
        Dumps::State(s, e, c, d) => {
            let s = s.as_slice();
            let c = c.as_slice();
            match (s, c, *d) {
                ([.., res], [CodeElem::Ret], Dumps::Halt) => Ok(real((*res).clone())),
                (s, c, d) => {
                    run(step(Dumps::State(s.to_vec(), e, c.to_vec(), Box::new(d)))?)
                }
            }
        }
    }
}


#[cfg(test)]
mod tests {
    //use crate::lambda::*;
    use crate::secd::*;

    #[test]
    fn step_const_test() {
        let id = Term::Abst("x".to_string(), Box::new(Term::Var("x".to_string()))); // (lambda x.x)
        let idapp = Term::App(Box::new(id), Box::new(Term::Const(Constants::Int(42)))); // f x
        let code = vec![CodeElem::Ret, CodeElem::LTerm(idapp)];

        let res = step(Dumps::State(vec![], vec![], code, Box::new(Dumps::Halt))).unwrap();
        assert_eq!(res.to_string(),
                   "S = (), E = (), C = (call :: Lambda x.x :: Int 42), D = halt\n");
        let res = step(res).unwrap();
        assert_eq!(res.to_string(),
                   "S = ((Int 42, ())), E = (), C = (call :: Lambda x.x), D = halt\n");
        let res = step(res).unwrap();
        assert_eq!(res.to_string(),
                   "S = ((Int 42, ()) :: (Lambda x.x, ())), E = (), C = (call), D = halt\n");
        let res = step(res).unwrap();
        assert_eq!(res.to_string(),
                   "S = (), E = ((x, (Int 42, ()))), C = (ret :: x), D = halt\n");
        let res = step(res).unwrap();
        assert_eq!(res.to_string(),
                   "S = ((Int 42, ())), E = ((x, (Int 42, ()))), C = (ret), D = halt\n");
    }
    #[test]
    fn step_abst_test() {
        let id = Term::Abst("x".to_string(), Box::new(Term::Var("x".to_string()))); // (lambda x.x)
        let fappx = Term::App(Box::new(Term::Var("f".to_string())), Box::new(Term::Var("x".to_string()))); // f x
        let fx = Term::Abst("f".to_string(),
                            Box::new(Term::Abst("x".to_string(),
                                                Box::new(fappx))));
        let code = vec![CodeElem::Ret, CodeElem::LTerm(Term::App(Box::new(fx), Box::new(id)))];
        let res = step(Dumps::State(vec![], vec![], code, Box::new(Dumps::Halt))).unwrap();
        assert_eq!(res.to_string(),
                   "S = (), E = (), C = (call :: Lambda f.Lambda x.(f) (x) :: Lambda x.x), D = halt\n");
        let res = step(res).unwrap();
        assert_eq!(res.to_string(),
                   "S = ((Lambda x.x, ())), E = (), C = (call :: Lambda f.Lambda x.(f) (x)), D = halt\n");
        let res = step(res).unwrap();
        assert_eq!(res.to_string(),
                   "S = ((Lambda x.x, ()) :: (Lambda f.Lambda x.(f) (x), ())), E = (), C = (call), D = halt\n");
        let res = step(res).unwrap();
        assert_eq!(res.to_string(),
                   "S = (), E = ((f, (Lambda x.x, ()))), C = (ret :: Lambda x.(f) (x)), D = halt\n");
        let res = step(res).unwrap();
        assert_eq!(res.to_string(),
                   "S = ((Lambda x.(f) (x), ((f, (Lambda x.x, ()))))), E = ((f, (Lambda x.x, ()))), C = (ret), D = halt\n");
    }

    #[test]
    fn run_test1() {
        let id = Term::Abst("x".to_string(), Box::new(Term::Var("x".to_string()))); // (lambda x.x)
        let fappx = Term::App(Box::new(Term::Var("f".to_string())), Box::new(Term::Var("x".to_string()))); // f x
        let fx = Term::Abst("f".to_string(),
                            Box::new(Term::Abst("x".to_string(),
                                                Box::new(fappx))));
        let fxappid = Term::App(Box::new(fx), Box::new(id));
        let app42 = Term::App(Box::new(fxappid), Box::new(Term::Const(Constants::Int(42))));
        let code = vec![CodeElem::Ret, CodeElem::LTerm(app42)];

        let res = run(Dumps::State(vec![], vec![], code, Box::new(Dumps::Halt))).unwrap();
        assert_eq!(res.to_string(), "Int 42");
    }
    #[test]
    fn run_test2() {
        let id = Term::Abst("x".to_string(), Box::new(Term::Var("x".to_string()))); // (lambda x.x)
        let fappx = Term::App(Box::new(Term::Var("f".to_string())), Box::new(Term::Var("x".to_string()))); // f x
        let fx = Term::Abst("f".to_string(),
                            Box::new(Term::Abst("x".to_string(),
                                                Box::new(fappx))));
        let fxappid = Term::App(Box::new(fx), Box::new(id));
        let code = vec![CodeElem::Ret, CodeElem::LTerm(fxappid)];

        let res = run(Dumps::State(vec![], vec![], code, Box::new(Dumps::Halt))).unwrap();
        assert_eq!(res.to_string(), "Lambda x.(Lambda x.x) (x)")
    }
}
