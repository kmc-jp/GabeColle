
#[macro_use]
extern crate err_derive;

pub mod errors;
pub mod lexer;
pub mod sexp;
pub mod parser;

#[cfg(test)]
mod tests {
    use crate::lexer::*;
    use crate::parser::*;

    #[test]
    fn parser_lexer_test1() {
        let res = parse_tokens(&analyze("((lambda (x) x) 10)".to_string())).unwrap();
        assert_eq!(res.to_string(),
                   "((Symbol(lambda) . ((Symbol(x) . Nil) . (Symbol(x) . Nil))) . (Int(10) . Nil))");
    }

    #[test]
    fn parser_lexer_test_nil1() {
        let res = parse_tokens(&analyze("()".to_string())).unwrap();
        assert_eq!(res.to_string(), "Nil");
    }

    // ???
    #[test]
    fn parser_lexer_test_nil2() {
        let res = parse_tokens(&analyze("(a  ())".to_string())).unwrap();
        assert_eq!(res.to_string(), "(Symbol(a) . (Nil . Nil))");
    }

    #[test]
    #[should_panic]
    fn parser_lexer_test_missing_rp() {
        // missing right paren
        parse_tokens(&analyze("(hoge".to_string())).unwrap();
    }

    #[test]
    #[should_panic]
    fn parser_lexer_test_unexpected_rp() {
        parse_tokens(&analyze(")hoge)".to_string())).unwrap();
    }
}
