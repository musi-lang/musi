use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::io;
use std::result::Result as StdResult;

#[derive(Debug, PartialEq, Eq)]
pub struct DiaggenError(pub String);

pub type DiaggenResult<T = ()> = StdResult<T, DiaggenError>;

impl Display for DiaggenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.0.as_str())
    }
}

impl Error for DiaggenError {}

pub fn io_error(action: &'static str) -> impl FnOnce(io::Error) -> DiaggenError {
    move |source| DiaggenError(format!("{action} failed (`{source}`)"))
}
