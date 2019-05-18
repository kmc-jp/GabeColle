//
// lambda term
//

#[derive(Clone)]
pub enum Constants {
    Int(i32),
}
pub type Variable = String;

#[derive(Clone)]
pub enum Values {
    Const(Constants),
    Var(Variable),
    Abst(Variable, Box<Term>)
}

#[derive(Clone)]
pub enum Answers {
    Const(Constants),
    Abst(Variable, Box<Term>)
}

#[derive(Clone)]
pub enum Term {
    Const(Constants),
    Var(Variable),
    Abst(Variable, Box<Term>),
    App(Box<Term>, Box<Term>),
}


impl Constants {
    pub fn to_string(&self) -> String {
        use Constants::*;
        match *self {
            Int(i) => "Int ".to_string() + &i.to_string()
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
            Abst(ref var, ref t) => "Lambda ".to_string() + &var.to_string() + "." + &t.to_string()
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
            Abst(ref v, ref t2) => "Lambda ".to_string() + &v + "." + &t2.to_string(),
            App(ref t1, ref t2) => "(".to_string() + &t1.to_string() + ") (" + &t2.to_string() + ")"
        }
    }
}

// todo
pub fn delta(_c1: Constants, _c2: Constants) -> Answers {
    Answers::Const(Constants::Int(42))
}

#[cfg(test)]
mod tests {
    use crate::lambda::*;

    #[test]
    fn print_lambda() {
        // Constants
        let c1 = Constants::Int(1);
        assert_eq!(c1.to_string(), "Int 1");

        // Answers
        let a1 = Answers::Const(Constants::Int(2));
        let a2 = Answers::Abst("x".to_string(), Box::new(Term::Var("x".to_string())));
        assert_eq!(a1.to_string(), "Int 2");
        assert_eq!(a2.to_string(), "Lambda x.x");

        // Term
        let t1 = Term::Var("x".to_string());
        assert_eq!(t1.to_string(), "x");
        let t2 = Box::new(Term::Abst("x".to_string(), Box::new(t1)));
        let t3 = Box::new(Constants::Int(3).to_term());
        assert_eq!(Term::App(t2, t3).to_string(), "(Lambda x.x) (Int 3)");
    }
}
