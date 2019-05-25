
extern crate gabecolle;

use std::fs;
use gabecolle::*;

#[test]
fn example_lisp_test() {
    let code = fs::read_to_string("tests/layer0.lisp").unwrap();
    let tokens = lexer::analyze(code).unwrap();
    eprintln!("{}", lexer::string_of_tokens(&tokens));
    let term = parser::parse(&tokens).unwrap();
    eprintln!("{}", term.to_string());
}
