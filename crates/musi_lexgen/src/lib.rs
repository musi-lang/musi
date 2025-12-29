mod lexer;

use proc_macro::TokenStream;

/// Builds lexer for enum.
///
/// Use `#[token("...")]` for exact matches.
///
/// Use `#[lexer(skip = r"...")]` for text to ignore (like spaces).
#[proc_macro_derive(Lexer, attributes(lexer, token, regex, callback))]
pub fn derive_lexer(input: TokenStream) -> TokenStream {
    lexer::derive_lexer_impl(input.into())
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}
