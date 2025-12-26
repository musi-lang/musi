use crate::token::Token;
use musi_basic::diagnostic::DiagnosticBag;

// ============================================================================
// LEXER
// ============================================================================

pub type TokenStream = (Vec<Token>, DiagnosticBag);
