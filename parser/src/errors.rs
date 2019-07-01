
#[derive(Debug, Error)]
pub enum ParserError {
    #[error(display = "Missing ')'")]
    MissingRP,
    #[error(display = "Unexpected ')'")]
    UnexpectedRP,
    #[error(display = "Expected ')")]
    ExpectedRP,
    #[error(display = "Empty token")]
    EmptyToken,
}

