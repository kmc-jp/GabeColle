
#[derive(Debug, Error)]
pub enum SECDError {
    #[error(display = "Does not match with delta rules")]
    DeltaUnmatch,

    #[error(display = "Unimplemented")]
    Unimplemented,
}

