use crate::token::Token;
use musi_basic::diagnostic::DiagnosticBag;

pub type Tokens = Vec<Token>;
pub type TokenStream = (Tokens, DiagnosticBag);
