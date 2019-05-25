#![feature(slice_patterns)]

#[macro_use]
extern crate err_derive;

pub mod errors;
pub mod lexer;
pub mod parser;
pub mod lambda;
pub mod codegen;
pub mod secd;
