
#[derive(Debug, Clone)]
pub enum SExp {
    AtomNil,
    AtomStr(String),
    AtomInt(i32),
    Cell(Box<SExp>, Box<SExp>)
}

impl SExp {
    pub fn to_string(&self) -> String {
        use SExp::*;
        match self {
            AtomNil => "Nil".to_string(),
            AtomStr(s) => format!("Sym({})", s),
            AtomInt(i) => format!("Int({})", i),
            Cell(e1, e2) => format!("({} . {})", (*e1).to_string(), (*e2).to_string())
        }
    }
}

