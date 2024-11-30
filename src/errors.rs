#[derive(Debug)]
pub enum MusiError {
    Lexical(LexicalError),
    Syntax(SyntaxError),
}

#[derive(Debug)]
pub struct LexicalError {
    pub message: &'static str,
}

#[derive(Debug)]
pub struct SyntaxError {
    pub message: &'static str,
}

pub type MusiResult<T> = Result<T, MusiError>;

impl std::error::Error for MusiError {}

impl std::fmt::Display for MusiError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lexical(error) => write!(f, "Lexical Error: {}", error.message),
            Self::Syntax(error) => write!(f, "Syntax Error: {}", error.message),
        }
    }
}
