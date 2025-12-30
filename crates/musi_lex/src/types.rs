use musi_errors::DiagnosticBag;

use crate::token::Token;

pub type TokenStream = (Vec<Token>, DiagnosticBag);
