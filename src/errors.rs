
#[derive(Debug, Error)]
pub enum EvalError {
    #[error(display = "variable not fonnd: {:?}", _0)]
    VariableNotFound(String),
    #[error(display = "attempt to return empty stack")]
    EmptyStack,
    #[error(display = "attempt to pop empty code")]
    EmptyCode,
    #[error(display = "unmatch delta rules")]
    DeltaUnmatch,
    #[error(display = "fatal error")]
    FatalError,
}

