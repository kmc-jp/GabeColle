//
// lambda term
//

#[derive(Clone)]
pub enum Constants {
    Int(i32),
}
pub type Variable = String;
//pub type LambdaAbst = (Variable, Box<Term>);

#[derive(Clone)]
pub enum Answers {
    Const(Constants),
    Abst(Variable, Box<Term>)
}

#[derive(Clone)]
pub enum Values {
    Var(Variable),
    Ans(Answers)
}

#[derive(Clone)]
pub enum Term {
    Val(Values),
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

    pub fn to_value(&self) -> Values {
        Values::Ans(self.to_ans())
    }

    pub fn to_term(&self) -> Term {
        Term::Val(self.to_value())
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
        Values::Ans(self.clone())
    }

    pub fn to_term(&self) -> Term {
        Term::Val(self.to_value())
    }
}

impl Values {
    pub fn to_string(&self) -> String {
        use Values::*;
        match *self {
            Var(ref var) => var.to_string(),
            Ans(ref ans) => ans.to_string()
        }
    }

    pub fn to_term(&self) -> Term {
        Term::Val(self.clone())
    }
}

impl Term {
    pub fn to_string(&self) -> String {
        use Term::*;
        match *self {
            Val(ref v) => v.to_string(),
            App(ref t1, ref t2) => "(".to_string() + &t1.to_string() + ") (" + &t2.to_string() + ")"
        }
    }
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
        let a2 = Answers::Abst("x".to_string(), Box::new(Values::Var("x".to_string()).to_term()));
        assert_eq!(a1.to_string(), "Int 2");
        assert_eq!(a2.to_string(), "Lambda x.x");

        // Values
        let v1 = Values::Var("x".to_string());
        let v2 = Values::Ans(a2);
        assert_eq!(v1.to_string(), "x");
        assert_eq!(v2.to_string(), "Lambda x.x");

        // Term
        let t1 = Term::Val(Values::Var("x".to_string()));
        assert_eq!(t1.to_string(), "x");
        let t2 = Box::new(Answers::Abst("x".to_string(), Box::new(t1)).to_term());
        let t3 = Box::new(Constants::Int(3).to_term());
        assert_eq!(Term::App(t2, t3).to_string(), "(Lambda x.x) (Int 3)");
    }
}
