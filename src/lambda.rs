//
// lambda term
//

use crate::errors::EvalError;
use crate::errors::EvalError::*;

#[derive(Debug, Clone)]
pub enum Constants {
    Nil,
    Dummy,
    Atom,
    Eq,
    Eq2(Box<Answers>),
    Car,
    Cdr,
    Cons,
    Cons2(Box<Answers>),
    If,
    IfTrue1,
    IfTrue2(Box<Answers>),
    IfFalse1,
    IfFalse2,
    Quote(Box<Answers>),
    Pair(Box<Answers>, Box<Answers>),
    Int(i32),
    Symbol(String),
}
pub type Variable = String;

#[derive(Debug, Clone)]
pub enum Values {
    Const(Constants),
    Var(Variable),
    Abst(Variable, Box<Term>)
}

#[derive(Debug, Clone)]
pub enum Answers {
    Const(Constants),
    Abst(Variable, Box<Term>)
}

#[derive(Debug, Clone)]
pub enum Term {
    Const(Constants),
    Var(Variable),
    Abst(Variable, Box<Term>),
    App(Box<Term>, Box<Term>),
}


impl Constants {
    pub fn to_string(&self) -> String {
        use Constants::*;
        match self {
            Nil => "Nil".to_string(),
            Dummy => "Dummy".to_string(),
            Atom => "Atom".to_string(),
            Eq => "Eq".to_string(),
            Eq2(a) => "Eq2(".to_string() + &(*a).to_string() + ")",
            Car => "Car".to_string(),
            Cdr => "Cdr".to_string(),
            Cons => "Cons".to_string(),
            Cons2(a) => "Cons2(".to_string() + &(*a).to_string() + ")",
            If => "If".to_string(),
            IfTrue1 => "IfTrue1".to_string(),
            IfTrue2(a) => "IfTrue2(".to_string() + &(*a).to_string() + ")",
            IfFalse1 => "IfFalse1".to_string(),
            IfFalse2 => "IfFalse2".to_string(),
            Quote(a) => "Quote(".to_string() + &a.to_string() + ")",
            Symbol(s) => "Symbol(".to_string() + &s + ")",
            Int(i) => "Int(".to_string() + &i.to_string() + ")",
            Pair(a1, a2) => "Pair(".to_string() + &a1.to_string() + ", " + &a2.to_string() + ")",
        }
    }

    pub fn to_ans(&self) -> Answers {
        Answers::Const(self.clone())
    }

    pub fn to_term(&self) -> Term {
        Term::Const(self.clone())
    }
}

impl Values {
    pub fn to_term(&self) -> Term {
        use Values::*;
        match self {
            Const(c) => Term::Const(c.clone()),
            Var(v) => Term::Var(v.clone()),
            Abst(v, t) => Term::Abst(v.clone(), t.clone()),
        }
    }
}

impl Answers {
    pub fn to_string(&self) -> String {
        use Answers::*;
        match *self {
            Const(ref c) => c.to_string(),
            Abst(ref var, ref t) => "(Lambda ".to_string() + &var.to_string() + "." + &t.to_string() + ")"
        }
    }

    pub fn to_value(&self) -> Values {
        use Answers::*;
        match self {
            Const(c) => Values::Const(c.clone()),
            Abst(v, t) => Values::Abst(v.clone(), t.clone())
        }
    }

    pub fn to_term(&self) -> Term {
        use Answers::*;
        match self {
            Const(c) => Term::Const(c.clone()),
            Abst(v, t) => Term::Abst(v.clone(), t.clone())
        }
    }
}

impl Term {
    pub fn to_string(&self) -> String {
        use Term::*;
        match *self {
            Const(ref v) => v.to_string(),
            Var(ref v) => v.to_string(),
            Abst(ref v, ref t2) => "(Lambda ".to_string() + &v + "." + &t2.to_string() + ")",
            App(ref t1, ref t2) => "(".to_string() + &t1.to_string() + " " + &t2.to_string() + ")"
        }
    }
}

// delta rules
pub fn delta(c: Constants, a: Answers) -> Result<Answers, EvalError> {
    use Constants::*;
    use Answers::*;

    eprintln!("delta: {}, {}", c.to_string(), a.to_string());

    let tr = Quote(Box::new(Const(Symbol("T".to_string()))));
    let nil = Quote(Box::new(Const(Symbol("nil".to_string()))));

    match (c, a) {
        (Atom, Const(Symbol(_)))  => Ok(Answers::Const(tr)),
        (Atom, _)                 => Ok(Const(nil)),
        (Car, Const(Pair(a1, _))) => Ok(*a1),
        (Car, Const(Quote(a)))    => Ok(Const(Quote(Box::new(delta(Car, *a)?)))),
        (Car, _)                  => Ok(Const(nil)),
        (Cdr, Const(Pair(_, a2))) => Ok(*a2),
        (Cdr, Const(Quote(a)))    => Ok(Const(Quote(Box::new(delta(Cdr, *a)?)))),
        (Cdr, _)                  => Ok(Const(nil)),
        (Cons, a)                 => Ok(Const(Cons2(Box::new(a)))),
        (Cons2(pa), b)            => Ok(Const(Pair(pa, Box::new(b)))),
        (Eq, a)                   => Ok(Const(Eq2(Box::new(a)))),
        (Eq2(pa), b)              => {
            match (*pa, b) {
                (Const(Symbol(ref s1)), Const(Symbol(ref s2))) if *s1 == *s2 => Ok(Const(tr)),
                _ => Ok(Const(nil))
            }
        }
        (If, Const(ref g)) if g.to_string() == nil.to_string() => Ok(Const(IfFalse1)),
        (If, _)                   => Ok(Const(IfTrue1)),
        (IfTrue1, a)              => Ok(Const(IfTrue2(Box::new(a)))),
        (IfTrue2(pa), _)          => Ok(*pa),
        (IfFalse1, _)             => Ok(Const(IfFalse2)),
        (IfFalse2, a)             => Ok(a),

        _ => Err(DeltaUnmatch),
    }
}

#[cfg(test)]
mod tests {
    use crate::lambda::*;

    #[test]
    fn print_lambda() {
        // Constants
        let c1 = Constants::Int(1);
        assert_eq!(c1.to_string(), "Int(1)");

        // Answers
        let a1 = Answers::Const(Constants::Int(2));
        let a2 = Answers::Abst("x".to_string(), Box::new(Term::Var("x".to_string())));
        assert_eq!(a1.to_string(), "Int(2)");
        assert_eq!(a2.to_string(), "(Lambda x.x)");

        // Term
        let t1 = Term::Var("x".to_string());
        assert_eq!(t1.to_string(), "x");
        let t2 = Box::new(Term::Abst("x".to_string(), Box::new(t1)));
        let t3 = Box::new(Constants::Int(3).to_term());
        assert_eq!(Term::App(t2, t3).to_string(), "((Lambda x.x) Int(3))");
    }
}
