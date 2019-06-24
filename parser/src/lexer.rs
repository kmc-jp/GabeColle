
use std::vec::Vec;

#[derive(Clone)]
pub enum Token {
    LP,
    RP,
    Symbol(String),
    Int(i32),
}

impl Token {
    pub fn to_string(&self) -> String {
        use Token::*;
        match self {
            LP => "LP".to_string(),
            RP => "RP".to_string(),
            Symbol(s) => format!("Symbol({})", s),
            Int(i) => format!("Int({})", i)
        }
    }
}

pub fn string_of_tokens(ts: &Vec<Token>) -> String {
    ts.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(" ")
}

fn token_of_string(s: &String) -> Option<Token> {
    use Token::*;
    match s.as_str() {
        "(" => Some(LP),
        ")" => Some(RP),
        s if !s.is_empty() => {
            match s.parse::<i32>() {
                Ok(i)  => Some(Int(i)),
                Err(_) => Some(Symbol(s.to_string())),
            }
        }
        _ => None
    }
}

pub fn analyze(s: String) -> Vec<Token> {
    let mut cur: String = String::new();
    let mut res: Vec<Token> = Vec::new();
    let mut is_comment = false;
    for c in s.to_lowercase().chars() {
        if is_comment  { // ignore characters until endline
            is_comment = c != '\n';
            continue;
        }
        is_comment = c == ';';
        if c == ' ' || c == ';' || c == '\n' {
            if let Some(token) = token_of_string(&cur) {
                res.push(token);
            }
            cur.clear();
        } else if c == '(' || c == ')' {
            if let Some(token) = token_of_string(&cur) {
                res.push(token);
            }
            cur.clear();
            if c == '(' { res.push(Token::LP); }
            else        { res.push(Token::RP); }
        } else {
            cur.push(c);
        }
    }
    if let Some(token) = token_of_string(&cur) {
        res.push(token)
    }

    res
}


// -----------------------------------------------------------------------------
// Lexer tests
// -----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::lexer::*;

    #[test]
    fn lexer_test1() {
        let s = "(lambda (x) x) y".to_string();
        let res = analyze(s);
        assert_eq!(string_of_tokens(&res), "LP Symbol(lambda) LP Symbol(x) RP Symbol(x) RP Symbol(y)");
    }
    #[test]
    fn lexer_test_upper() {
        let s = "(LAMBDA (x) X) Y".to_string();
        let res = analyze(s);
        assert_eq!(string_of_tokens(&res), "LP Symbol(lambda) LP Symbol(x) RP Symbol(x) RP Symbol(y)");
    }
    #[test]
    fn lexer_test_int() {
        let s = "(LAMBDA (X) x) 42".to_string();
        let res = analyze(s);
        assert_eq!(string_of_tokens(&res), "LP Symbol(lambda) LP Symbol(x) RP Symbol(x) RP Int(42)");
    }
    #[test]
    fn lexer_test_comment() {
        let s = "(LAMBDA (x) \n x) ; identity-fuction ()\n 42".to_string();
        let res = analyze(s);
        assert_eq!(string_of_tokens(&res), "LP Symbol(lambda) LP Symbol(x) RP Symbol(x) RP Int(42)");
    }
}
