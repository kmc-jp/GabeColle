
use std::vec::Vec;
use crate::lexer::*;
use crate::errors::*;
use crate::sexp::*;

// sexp     -> "(" sexp_seq ")" | ...
// sexp_seq -> sexp*

fn parse_sexp_seq(ts: &Vec<Token>, idx: &mut usize) -> Result<SExp, ParserError> {
    if *idx >= ts.len() { return Err(ParserError::MissingRP) }

    if let Token::RP = ts[*idx] {
        Ok(SExp::AtomNil)
    } else {
        let car = Box::new(parse_sexp(&ts, idx)?);
        let cdr = Box::new(parse_sexp_seq(&ts, idx)?);
        Ok(SExp::Cell(car, cdr))
    }
}

fn parse_sexp(ts: &Vec<Token>, idx: &mut usize) -> Result<SExp, ParserError> {
    match &ts[*idx] {
        Token::LP => {
            *idx = *idx + 1; // LP
            let seq = parse_sexp_seq(&ts, idx);
            *idx = *idx + 1; // RP
            seq
        },
        Token::Int(i) => {
            *idx = *idx + 1;
            Ok(SExp::AtomInt(*i))
        },
        Token::Symbol(s) => {
            *idx = *idx + 1;
            Ok(SExp::AtomStr(s.clone()))
        },

        Token::RP => Err(ParserError::UnexpectedRP)
    }
}

pub fn parse_tokens(ts: &Vec<Token>) -> Result<SExp, ParserError> {
    if ts.len() == 0 { return Err(ParserError::EmptyToken) }
    let mut idx = 0;
    parse_sexp(&ts, &mut idx)
}

// -----------------------------------------------------------------------------
// Parser tests
// -----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::parser::*;

    #[test]
    fn parser_test1() {
        // (lambda (x) x 10)
        let ts = vec![Token::LP,
                      Token::Symbol("lambda".to_string()),
                      Token::LP,
                      Token::Symbol("x".to_string()),
                      Token::RP,
                      Token::Symbol("x".to_string()),
                      Token::Int(10),
                      Token::RP];
        assert_eq!(parse_tokens(&ts).unwrap().to_string(),
                  "(Symbol(lambda) . ((Symbol(x) . Nil) . (Symbol(x) . (Int(10) . Nil))))");
    }
}
