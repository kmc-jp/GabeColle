
use parser::sexp;
use crate::errors::SECDError;

pub type Variable = String;

#[derive(Debug, Clone)]
pub enum Constants {
    Dummy,
    True,
    Nil,
    Int(i32),
    Symbol(String),
    SExp(sexp::SExp),
    Pair(Box<Answers>, Box<Answers>),

    Atom,
    Eq2,
    Eq1(Box<Answers>),
    Car,
    Cdr,
    Cons2,
    Cons1(Box<Answers>),
    If,
    IfTrue2,
    IfTrue1(Box<Answers>),
    IfFalse2,
    IfFalse1,
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
    App(Box<Term>, Box<Term>)
}


impl Constants {
    pub fn to_answer(self) -> Answers {
        Answers::Const(self)
    }

    pub fn to_string(&self) -> String {
        use Constants::*;
        match self {
            Dummy => "Dummy".to_string(),
            True  => "True".to_string(),
            Nil   => "Nil".to_string(),
            Int(i) => i.to_string(),
            Symbol(s) => format!("Sym({})", s),
            SExp(sexp) => sexp.to_string(),
            Pair(pa1, pa2) => format!("({} . {})", pa1.to_string(), pa2.to_string()),
            Atom => "Atom".to_string(),
            Eq2  => "Eq2".to_string(),
            Eq1(pa) => format!("Eq1({})", pa.to_string()),
            Car => "Car".to_string(),
            Cdr => "Cdr".to_string(),
            Cons2 => "Cons2".to_string(),
            Cons1(pa) => format!("Cons1({})", pa.to_string()),
            If => "If".to_string(),
            IfTrue2 => "IfTrue2".to_string(),
            IfTrue1(pa) => format!("IfTrue1({})", pa.to_string()),
            IfFalse2 => "IfFalse2".to_string(),
            IfFalse1 => "IfFalse1".to_string(),
        }
    }
}

impl Answers {
    pub fn to_string(&self) -> String {
        use Answers::*;
        match self {
            Const(c) => c.to_string(),
            Abst(var, t) => format!("(L_{}.{}", var, t.to_string())
        }
    }
}

impl Term {
    pub fn to_string(&self) -> String {
        use Term::*;
        match self {
            Const(c) => c.to_string(),
            Var(var) => var.clone(),
            Abst(var, t) => format!("(L_{}.{})", var, t.to_string()),
            App(t1, t2) => format!("{} {}", t1.to_string(), t2.to_string())
        }
    }
}


// -----------------------------------------------------------------------------
// delta rules
// -----------------------------------------------------------------------------

fn delta_atom(a: Answers) -> Result<Answers, SECDError> {
    match a {
        Answers::Const(Constants::Symbol(_)) => Ok(Answers::Const(Constants::True)),
        Answers::Abst(_, _)                  => Ok(Answers::Const(Constants::True)),
        _ => Ok(Answers::Const(Constants::Nil))
    }
}

fn delta_car(a: Answers) -> Result<Answers, SECDError> {
    match a {
        Answers::Const(Constants::Pair(a1, _)) => Ok(*a1),
        _ => Ok(Answers::Const(Constants::Nil))
    }
}

fn delta_cdr(a: Answers) -> Result<Answers, SECDError> {
    match a {
        Answers::Const(Constants::Pair(_, a2)) => Ok(*a2),
        _ => Ok(Answers::Const(Constants::Nil))
    }
}

fn delta_cons2(a: Answers) -> Result<Answers, SECDError> {
    Ok(Answers::Const(Constants::Cons1(Box::new(a))))
}

fn delta_cons1(pa: Box<Answers>, a: Answers) -> Result<Answers, SECDError> {
    Ok(Answers::Const(Constants::Pair(pa, Box::new(a))))
}

fn delta_eq2(a: Answers) -> Result<Answers, SECDError> {
    Ok(Answers::Const(Constants::Eq1(Box::new(a))))
}

fn delta_eq1(pa: Box<Answers>, a: Answers) -> Result<Answers, SECDError> {
    if let Answers::Const(Constants::Symbol(s1)) = *pa {
        if let Answers::Const(Constants::Symbol(s2)) = a {
            if s1 == s2 {
                Ok(Answers::Const(Constants::True))
            } else {
                Ok(Answers::Const(Constants::Nil))
            }
        } else {
            Ok(Answers::Const(Constants::Nil))
        }
    } else {
        Ok(Answers::Const(Constants::Nil))
    }
}

fn delta_if(a: Answers) -> Result<Answers, SECDError> {
    match a {
        Answers::Const(Constants::Nil) => Ok(Answers::Const(Constants::IfFalse2)),
        _ => Ok(Answers::Const(Constants::IfTrue2))
    }
}

pub fn delta(c: Constants, a: Answers) -> Result<Answers, SECDError> {
    use Constants::*;
    match c {
        Atom        => delta_atom(a),
        Eq2         => delta_eq2(a),
        Eq1(pa)     => delta_eq1(pa, a),
        Car         => delta_car(a),
        Cdr         => delta_cdr(a),
        Cons2       => delta_cons2(a),
        Cons1(pa)   => delta_cons1(pa, a),
        If          => delta_if(a),
        IfTrue2     => Ok(Answers::Const(Constants::IfTrue1(Box::new(a)))),
        IfTrue1(pa) => Ok(*pa),
        IfFalse2    => Ok(Answers::Const(Constants::IfFalse1)),
        IfFalse1    => Ok(a),

        _ => Err(SECDError::DeltaUnmatch)
    }
}


#[cfg(test)]
mod tests {
    use crate::lambda::*;

    #[test]
    fn test_delta_atom_nil() {
        let nil = Box::new(Constants::Nil.to_answer());
        let a = Answers::Const(Constants::Pair(nil.clone(), nil.clone()));
        assert_eq!("Nil", delta(Constants::Atom, a).unwrap().to_string());
    }
    #[test]
    fn test_delta_atom_pair() {
        let nil = Box::new(Constants::Nil.to_answer());
        let a = Constants::Pair(nil.clone(), nil.clone()).to_answer();
        assert_eq!("Nil", delta(Constants::Atom, a).unwrap().to_string());
    }
    #[test]
    fn test_delta_atom_symbol() {
        let a = Constants::Symbol("test".to_string()).to_answer();
        assert_eq!("True", delta(Constants::Atom, a).unwrap().to_string());
    }
    #[test]
    fn test_delta_atom_abst() {
        let a = Answers::Abst("x".to_string(), Box::new(Term::Var("x".to_string())));
        assert_eq!("True", delta(Constants::Atom, a).unwrap().to_string());
    }

    #[test]
    fn test_delta_car_nil() {
        let a = Constants::Nil.to_answer();
        assert_eq!("Nil", delta(Constants::Car, a).unwrap().to_string());
    }
    #[test]
    fn test_delta_car_pair() {
        let x = Constants::Symbol("x".to_string()).to_answer();
        let y = Constants::Symbol("y".to_string()).to_answer();
        let p = Constants::Pair(Box::new(x), Box::new(y)).to_answer();
        assert_eq!("Sym(x)", delta(Constants::Car, p).unwrap().to_string());
    }

    #[test]
    fn test_delta_cdr_nil() {
        let a = Constants::Nil.to_answer();
        assert_eq!("Nil", delta(Constants::Cdr, a).unwrap().to_string());
    }
    #[test]
    fn test_delta_cdr_pair() {
        let x = Constants::Symbol("x".to_string()).to_answer();
        let y = Constants::Symbol("y".to_string()).to_answer();
        let z = Constants::Symbol("z".to_string()).to_answer();
        let p2 = Constants::Pair(Box::new(y), Box::new(z)).to_answer();
        let p = Constants::Pair(Box::new(x), Box::new(p2)).to_answer();
        assert_eq!("(Sym(y) . Sym(z))", delta(Constants::Cdr, p).unwrap().to_string());
    }

    #[test]
    fn test_delta_cons2() {
        let a = Constants::Nil.to_answer();
        assert_eq!("Cons1(Nil)", delta(Constants::Cons2, a).unwrap().to_string());
    }

    #[test]
    fn test_delta_cons1() {
        let nil = Constants::Nil.to_answer();
        let cons1 = Constants::Cons1(Box::new(Constants::Nil.to_answer()));
        assert_eq!("(Nil . Nil)", delta(cons1, nil).unwrap().to_string());
    }

    #[test]
    fn test_delta_eq2() {
        let nil = Constants::Nil.to_answer();
        assert_eq!("Eq1(Nil)", delta(Constants::Eq2, nil).unwrap().to_string());
    }

    #[test]
    fn test_delta_eq1_nil() {
        let nil = Constants::Nil.to_answer();
        let eq1 = Constants::Eq1(Box::new(nil.clone()));
        assert_eq!("Nil", delta(eq1, nil).unwrap().to_string());
    }
    #[test]
    fn test_delta_eq1_abst() {
        let abst = Answers::Abst("x".to_string(), Box::new(Term::Var("x".to_string())));
        let eq1 = Constants::Eq1(Box::new(abst.clone()));
        assert_eq!("Nil", delta(eq1, abst).unwrap().to_string());
    }
    #[test]
    fn test_delta_eq1_symbol_false() {
        let x = Constants::Symbol("x".to_string()).to_answer();
        let y = Constants::Symbol("y".to_string()).to_answer();
        let eq1 = Constants::Eq1(Box::new(x));
        assert_eq!("Nil", delta(eq1, y).unwrap().to_string());
    }
    #[test]
    fn test_delta_eq1_symbol_true() {
        let x = Constants::Symbol("x".to_string()).to_answer();
        let eq1 = Constants::Eq1(Box::new(x.clone()));
        assert_eq!("True", delta(eq1, x).unwrap().to_string());
    }

    #[test]
    fn test_delta_if_nil() {
        let nil = Constants::Nil.to_answer();
        assert_eq!("IfFalse2", delta(Constants::If, nil).unwrap().to_string());
    }
    #[test]
    fn test_delta_if_true() {
        let t = Constants::True.to_answer();
        assert_eq!("IfTrue2", delta(Constants::If, t).unwrap().to_string());
    }
    #[test]
    fn test_delta_if_symbol() {
        let s = Constants::Symbol("x".to_string()).to_answer();
        assert_eq!("IfTrue2", delta(Constants::If, s).unwrap().to_string());
    }
}
