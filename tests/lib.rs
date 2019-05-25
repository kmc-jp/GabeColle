
extern crate gabecolle;

use std::fs;
use gabecolle::*;

fn calc(code: String) -> String {
    let tokens = lexer::analyze(code).unwrap();
    eprintln!("tokens: {}", lexer::string_of_tokens(&tokens));
    let term = parser::parse(&tokens).unwrap();
    eprintln!("term: {}", term.to_string());
    let initial = codegen::generate(term);
    secd::run(initial).unwrap().to_string()
}

#[test]
fn atom_test1() {
    assert_eq!(calc("(atom X)".to_string()), "Quote(Symbol(T))");
}

#[test]
fn atom_test2() {
    assert_eq!(calc("(atom (lambda (x) x))".to_string()), "Quote(Symbol(nil))");
}

#[test]
fn car_test1() {
    assert_eq!(calc("(car (cons X Y))".to_string()), "Symbol(X)");
}

#[test]
fn car_test2() {
    assert_eq!(calc("(car X)".to_string()), "Quote(Symbol(nil))");
}

#[test]
fn car_test3() {
    assert_eq!(calc("(car (quote (a b)))".to_string()), "Quote(Symbol(a))");
}

// @todo
#[test]
fn car_test4() {
    assert_eq!(calc("(car (quote a))".to_string()), "Quote(Quote(Symbol(nil)))");
}

#[test]
fn cdr_test1() {
    assert_eq!(calc("(cdr (cons X (cons Y Z)))".to_string()), "Pair(Symbol(Y), Symbol(Z))");
}

#[test]
fn cdr_test2() {
    assert_eq!(calc("(cdr X)".to_string()), "Quote(Symbol(nil))");
}

#[test]
fn cdr_test3() {
    assert_eq!(calc("(cdr (quote (a b c)))".to_string()), "Quote(Pair(Symbol(b), Symbol(c)))");
}

// @todo
#[test]
fn cdr_test4() {
    assert_eq!(calc("(cdr (quote (a)))".to_string()), "Quote(Quote(Symbol(nil)))");
}

#[test]
fn cons_test() {
    assert_eq!(calc("(cons (cons (lambda (x) x) Y) Z)".to_string()),
               "Pair(Pair((Lambda x.x), Symbol(Y)), Symbol(Z))");
}

#[test]
fn eq_test1() {
    assert_eq!(calc("(eq X X)".to_string()), "Quote(Symbol(T))");
}

#[test]
fn eq_test2() {
    assert_eq!(calc("(eq X Y)".to_string()), "Quote(Symbol(nil))");
}

#[test]
fn eq_test3() {
    assert_eq!(calc("(eq X (quote X))".to_string()), "Quote(Symbol(nil))");
}

#[test]
fn eq_test4() {
    assert_eq!(calc("(eq (quote X) (quote X))".to_string()), "Quote(Symbol(nil))");
}

#[test]
fn if_test1() {
    assert_eq!(calc("(if (eq X X) Y Z)".to_string()), "Symbol(Y)");
}

#[test]
fn if_test2() {
    assert_eq!(calc("(if (eq X Y) Y Z)".to_string()), "Symbol(Z)");
}

#[test]
#[should_panic]
fn quote_test1() {
    let _ = calc("(quote a b)".to_string());
}

// @todo
#[test]
fn quote_test2() {
    assert_eq!(calc("(quote ((lambda (x) x) Y))".to_string()),
               "Quote(Pair(Pair(Symbol(LAMBDA), Pair(Symbol(x), Symbol(x))), Symbol(Y)))");
}

#[test]
fn lambda_test1() {
    assert_eq!(calc("(lambda (x) x)".to_string()), "(Lambda x.x)");
}

#[test]
fn lambda_test2() {
    assert_eq!(calc("(lambda (x y) (x y))".to_string()), "(Lambda x.(Lambda y.(x y)))");
}

#[test]
#[should_panic]
fn lambda_test3() {
    let _ = calc("(lambda ((a b)) a b)".to_string());
}

#[test]
fn easy_test1() {
    assert_eq!(calc("((lambda (f x) (f x)) (lambda (x) x) I)".to_string()), "Symbol(I)");
}

#[test]
fn example_lisp_test() {
    let code = fs::read_to_string("tests/layer0.lisp").unwrap();
    let _ = calc(code); // do nothing
}
