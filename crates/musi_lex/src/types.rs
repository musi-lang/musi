use crate::token::Token;
use musi_basic::diagnostic::DiagnosticBag;

pub type TokenStream = (Vec<Token>, DiagnosticBag);
