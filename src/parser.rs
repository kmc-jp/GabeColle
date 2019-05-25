//
// layer0 lisp parser
//

use std::vec::Vec;

use super::lambda::*;
use super::lexer::Token;

#[derive(Debug, Error)]
pub enum ParserError {
    #[error(display = "expected sexp")]
    ExpectedSExp,
    #[error(display = "expected qexp")]
    ExpectedQExp,
    #[error(display = "expected '('")]
    ExpectedLP,
    #[error(display = "expected ')'")]
    ExpectedRP,
    #[error(display = "unexpected ')'")]
    UnexpectedRP,
    #[error(display = "unexpected lambda")]
    UnexpectedLambda,
    #[error(display = "unexpected quote")]
    UnexpectedQuote,
    #[error(display = "invalid argument")]
    InvalidArgs,
    #[error(display = "{:?} is not function", _0)]
    NotFunction(String),
}

fn lambda_args(ts: &Vec<Token>, idx: &mut usize) -> Result<Vec<Variable>, ParserError> { // ( args* )
    use ParserError::*;

    match &ts[*idx] {
        Token::LP => {
            *idx += 1;
            let mut res = Vec::new();
            while *idx < ts.len() {
                if let Token::RP = &ts[*idx] { break; }
                match &ts[*idx] {
                    Token::Symbol(s) => {
                        *idx += 1;
                        res.push((*s).clone());
                    },
                    _ => return Err(InvalidArgs)
                }
            }
            if ts.len() <= *idx { return Err(ExpectedRP) }
            if let Token::RP = &ts[*idx] {
                *idx += 1;
                Ok(res)
            } else {
                Err(ExpectedRP)
            }
        },
        _ => Err(ExpectedLP)
    }
}

fn build_lambda(ts: Vec<String>, body: Box<Term>) -> Term {
    match ts[..] {
        [] => Term::App(Box::new(Term::Abst("_".to_string(), body)), Box::new(Term::Const(Constants::Dummy))),
        [ref v] => Term::Abst(v.clone(), body),
        [ref v, ref vs..] => Term::Abst(v.clone(), Box::new(build_lambda(vs.to_vec(), body)))
    }
}

fn parse_sexp(ts: &Vec<Token>, idx: &mut usize) -> Result<Term, ParserError> {
    use ParserError::*;

    eprintln!("{}: {}", *idx, ts[*idx].to_string());

    if ts.len() <= *idx { return Err(ExpectedSExp) }

    if *idx + 1 < ts.len() { // nil
        if let (Token::LP, Token::RP) = (&ts[*idx], &ts[*idx + 1]) {
            *idx += 2;
            return Ok(Term::Const(Constants::Nil))
        }
    }

    match &ts[*idx] {
        Token::LP => {
            *idx += 1;
            if ts.len() <= *idx { return Err(ExpectedSExp) }
            match &ts[*idx] {
                Token::Lambda => {
                    *idx += 1;
                    let res = Ok(build_lambda(lambda_args(&ts, idx)?, Box::new(parse_sexp(&ts, idx)?)));
                    *idx += 1; // RP
                    res
                },
                Token::Quote => {
                    *idx += 1;
                    let res = Ok(Term::Const(Constants::Quote(Box::new(parse_qexp(&ts, idx)?))));
                    *idx += 1; // RP
                    res
                },
                _ => {
                    let mut res = parse_sexp(&ts, idx)?;
                    let mut is_apply_form = false;
                    while *idx < ts.len() {
                        if let Token::RP = &ts[*idx] { break; }
                        is_apply_form = true;
                        res = Term::App(Box::new(res), Box::new(parse_sexp(&ts, idx)?));
                    }
                    if !is_apply_form {
                        return Err(NotFunction(res.to_string()))
                    }
                    *idx += 1; // RP
                    Ok(res)
                }
            }
        },
        Token::Symbol(s) => { *idx += 1; Ok(Term::Const(Constants::Symbol(s.clone()))) }
        Token::Atom      => { *idx += 1; Ok(Term::Const(Constants::Atom)) },
        Token::Eq        => { *idx += 1; Ok(Term::Const(Constants::Eq)) },
        Token::Car       => { *idx += 1; Ok(Term::Const(Constants::Car)) },
        Token::Cdr       => { *idx += 1; Ok(Term::Const(Constants::Cdr)) },
        Token::Cons      => { *idx += 1; Ok(Term::Const(Constants::Cons)) },
        Token::If        => { *idx += 1; Ok(Term::Const(Constants::If)) },

        Token::RP => Err(UnexpectedRP),
        Token::Lambda => Err(UnexpectedLambda),
        Token::Quote => Err(UnexpectedQuote),
    }
}

fn create_quote_pairs(ans: Vec<Answers>) -> Answers {
    match &ans[..] {
        [] => Answers::Const(Constants::Nil),
        [a] => (*a).clone(),
        [a, tl..] => Answers::Const(Constants::Pair(Box::new((*a).clone()), Box::new(create_quote_pairs(tl.to_vec()))))
    }
}

fn parse_qexp(ts: &Vec<Token>, idx: &mut usize) -> Result<Answers, ParserError> {
    use ParserError::*;

    if ts.len() <= *idx { return Err(ExpectedSExp) }

    match &ts[*idx] { // ( qexp* )
        Token::LP => {
            *idx += 1;
            let mut ans: Vec<Answers> = Vec::new();
            while *idx < ts.len() {
                if let Token::RP = &ts[*idx] { break; }
                ans.push(parse_qexp(&ts, idx)?);
            }
            if ts.len() <= *idx { return Err(ExpectedRP) }
            match ts[*idx] {
                Token::RP => {
                    *idx += 1;
                    Ok(create_quote_pairs(ans))
                },
                _ => Err(ExpectedRP)
            }
        },

        Token::Symbol(s) => {
            *idx += 1;
            Ok(Answers::Const(Constants::Symbol(s.clone())))
        },

        Token::RP => Err(ExpectedQExp),

        Token::Atom
        | Token::Eq
        | Token::Car
        | Token::Cdr
        | Token::Cons
        | Token::If
        | Token::Quote
        | Token::Lambda => {
            *idx += 1;
            Ok(Answers::Const(Constants::Symbol(ts[*idx - 1].to_string())))
        }
    }
}

pub fn parse(ts: &Vec<Token>) -> Result<Term, ParserError> {
    let mut idx: usize = 0;
    parse_sexp(&ts, &mut idx)
}


// -----------------------------------------------------------------------------
// tests
// -----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_test_symbol() {
        let ts: Vec<Token> = vec![Token::Symbol("x".to_string())];
        assert_eq!(parse(&ts).unwrap().to_string(), "Symbol(x)");
    }

    #[test]
    fn parse_test_nil() {
        let ts: Vec<Token> = vec![Token::LP, Token::RP];
        assert_eq!(parse(&ts).unwrap().to_string(), "Nil");
    }

    #[test]
    fn parse_test_atom() {
        let ts: Vec<Token> = vec![Token::LP, Token::Atom, Token::Symbol("X".to_string()), Token::RP];
        assert_eq!(parse(&ts).unwrap().to_string(), "(Atom Symbol(X))");
    }

    #[test]
    fn parse_test_eq() {
        let ts: Vec<Token> = vec![Token::LP, Token::Eq, Token::Symbol("X".to_string()), Token::Symbol("Y".to_string()), Token::RP];
        assert_eq!(parse(&ts).unwrap().to_string(), "((Eq Symbol(X)) Symbol(Y))");
    }

    #[test]
    fn parse_test_car() {
        let ts: Vec<Token> = vec![Token::LP, Token::Car, Token::Symbol("X".to_string()), Token::RP];
        assert_eq!(parse(&ts).unwrap().to_string(), "(Car Symbol(X))");
    }

    #[test]
    fn parse_test_cdr() {
        let ts: Vec<Token> = vec![Token::LP, Token::Cdr, Token::Symbol("X".to_string()), Token::RP];
        assert_eq!(parse(&ts).unwrap().to_string(), "(Cdr Symbol(X))");
    }

    #[test]
    fn parse_test_cons() {
        let ts: Vec<Token> = vec![Token::LP, Token::Cons, Token::Symbol("X".to_string()), Token::Symbol("Y".to_string()), Token::RP];
        assert_eq!(parse(&ts).unwrap().to_string(), "((Cons Symbol(X)) Symbol(Y))");
    }

    #[test]
    fn parse_test_if() {
        let ts: Vec<Token> = vec![Token::LP, Token::If, Token::Symbol("X".to_string()), Token::Symbol("Y".to_string()), Token::Symbol("Z".to_string()), Token::RP];
        assert_eq!(parse(&ts).unwrap().to_string(), "(((If Symbol(X)) Symbol(Y)) Symbol(Z))");
    }

    #[test]
    fn parse_test_quote() {
        let ts: Vec<Token> = vec![Token::LP, Token::Quote, Token::LP, Token::Symbol("X".to_string()), Token::Symbol("Y".to_string()), Token::Cons, Token::RP, Token::RP];
        assert_eq!(parse(&ts).unwrap().to_string(), "Quote(Pair(Symbol(X), Pair(Symbol(Y), Symbol(CONS))))");
    }

    #[test]
    fn parse_test_lambda1() {
        let ts: Vec<Token> = vec![Token::LP, Token::Lambda, Token::LP, Token::Symbol("x".to_string()), Token::Symbol("y".to_string()), Token::RP, Token::LP, Token::Symbol("y".to_string()), Token::Symbol("x".to_string()), Token::RP, Token::RP];
        assert_eq!(parse(&ts).unwrap().to_string(), "(Lambda x.(Lambda y.(Symbol(y) Symbol(x))))");
    }

    #[test]
    fn parse_test_lambda2() { // empty arg
        let ts: Vec<Token> = vec![Token::LP, Token::Lambda, Token::LP, Token::RP, Token::LP, Token::RP, Token::RP];
        assert_eq!(parse(&ts).unwrap().to_string(), "((Lambda _.Nil) Dummy)");
    }

    #[test]
    fn parse_test_app1() {
        let ts: Vec<Token> = vec![Token::LP, Token::LP, Token::Lambda, Token::LP, Token::Symbol("x".to_string()), Token::RP, Token::Symbol("x".to_string()), Token::RP, Token::LP, Token::RP, Token::RP];
        assert_eq!(parse(&ts).unwrap().to_string(), "((Lambda x.Symbol(x)) Nil)");
    }

    #[test]
    fn parse_test_app2() {
        let ts: Vec<Token> = vec![Token::LP,
                                    Token::LP,
                                      Token::Lambda,
                                      Token::LP, Token::Symbol("x".to_string()), Token::Symbol("y".to_string()), Token::Symbol("z".to_string()), Token::RP,
                                      Token::LP, Token::RP,
                                    Token::RP,
                                    Token::Symbol("a1".to_string()),
                                    Token::Symbol("a2".to_string()),
                                    Token::Symbol("a3".to_string()),
                                  Token::RP];
        assert_eq!(parse(&ts).unwrap().to_string(),
                   "((((Lambda x.(Lambda y.(Lambda z.Nil))) Symbol(a1)) Symbol(a2)) Symbol(a3))");
    }
}

