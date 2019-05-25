//
// Lexer (Layer0 Lisp)
//

use std::vec::Vec;

#[derive(Debug, Error)]
pub enum LexerError {
    #[error(display = "unexpected string: {:?}", _0)]
    UnexpectedStr(String)
}

#[derive(Clone)]
pub enum Token {
    LP,     // (
    RP,     // )
    Atom,   // atom
    Eq,     // eq
    Cons,   // cons
    Car,    // car
    Cdr,    // cdr
    If,     // if
    Quote,  // quote
    Lambda, // lambda
    Symbol(String),
    Variable(String),
}

impl Token {
    pub fn to_string(&self) -> String {
        use Token::*;
        match self {
            LP => "LP".to_string(),
            RP => "RP".to_string(),
            Atom => "ATOM".to_string(),
            Eq => "EQ".to_string(),
            Cons => "CONS".to_string(),
            Car => "CAR".to_string(),
            Cdr => "CDR".to_string(),
            If => "IF".to_string(),
            Quote => "QUOTE".to_string(),
            Lambda => "LAMBDA".to_string(),
            Symbol(s) => format!("SYMBOL({})", s),
            Variable(s) => format!("VAR({})", s),
        }
    }
}

pub fn string_of_tokens(ts: &Vec<Token>) -> String {
    ts.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(" ")
}

fn token_of_string(s: &String) -> Result<Token, LexerError> {
    use Token::*;
    match s.as_str() {
        "(" => Ok(LP),
        ")" => Ok(RP),
        "atom" => Ok(Atom),
        "eq" => Ok(Eq),
        "cons" => Ok(Cons),
        "car" => Ok(Car),
        "cdr" => Ok(Cdr),
        "if" => Ok(If),
        "quote" => Ok(Quote),
        "lambda" => Ok(Lambda),
        t => {
            if is_symbol(t) {
                Ok(Symbol(s.clone()))
            } else if t.chars().all(char::is_alphanumeric) {
                Ok(Variable(s.clone()))
            } else {
                Err(LexerError::UnexpectedStr(s.clone()))
            }
        }
    }
}

fn is_ignore_character(c: char) -> bool {
    match c {
        ' ' | '\r' | '\t' | '\n' => true,
        _ => false
    }
}

fn is_symbol(s: &str) -> bool {
    if s.len() != 1 { return false }
    let caps = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let c = s.chars().nth(0).unwrap();
    caps.contains(c)
}

// @todo: support for multiline comments ?

fn skip_line(s: &Vec<char>, idx: &mut usize) {
    while *idx < s.len() {
        let c = s[*idx];
        *idx += 1;
        if c == '\n' {
            break;
        }
    }
}

pub fn analyze(s: String) -> Result<Vec<Token>, LexerError> {
    let s: Vec<char> = s.chars().collect(); // for accessing by index
    let mut cur: String = String::new();
    let mut res: Vec<Token> = Vec::new();
    let mut idx: usize = 0;
    while idx < s.len() {
        let c = s[idx];
        idx += 1;
        if cur.len() != 0 && (is_ignore_character(c) || c == '(' || c == ')' || c == ';') {
            res.push(token_of_string(&cur)?);
            cur.clear();
        }
        if is_ignore_character(c) { continue; }

        if c == ';' {
            skip_line(&s, &mut idx);
        } else if c == '(' {
            res.push(Token::LP);
        } else if c == ')' {
            res.push(Token::RP);
        } else {
            cur.push(c);
        }
    }
    if cur.len() != 0 {
        res.push(token_of_string(&cur)?);
    }

    Ok(res)
}


// -----------------------------------------------------------------------------
// test
// -----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::lexer::*;
    use std::fs;

    #[test]
    fn lexer_test1() {
        let tokens = analyze("(atom lambda \n \n XYZ \n eq car cdr if quote)".to_string()).unwrap();
        assert_eq!(string_of_tokens(&tokens),
                   "LP ATOM LAMBDA VAR(XYZ) EQ CAR CDR IF QUOTE RP");
    }

    #[test]
    fn lexer_symbol_var_test() {
        let tokens = analyze("x X yx YX".to_string()).unwrap();
        assert_eq!(string_of_tokens(&tokens), "VAR(x) SYMBOL(X) VAR(yx) VAR(YX)");
    }

    #[test]
    #[should_panic(expected = "lam-dab")]
    fn lexer_test_ng() {
        match analyze("atom lambda \n lam-dab".to_string()) {
            Ok(_) => {},
            Err(LexerError::UnexpectedStr(s)) => assert!(false, s)
        }
    }

    #[test]
    fn lexer_test_comment() {
        let tokens = analyze("(atom ; ahg ud 2 ga __ ;; s \n (lambda (x) x))".to_string()).unwrap();
        assert_eq!(string_of_tokens(&tokens), "LP ATOM LP LAMBDA LP VAR(x) RP VAR(x) RP RP");
    }

    #[test]
    #[ignore]
    fn lexer_sample_lisp() {
        let code = fs::read_to_string("src/layer0.lisp").unwrap();
        let tokens = analyze(code).unwrap();
        assert_eq!(string_of_tokens(&tokens), "");
    }
}
