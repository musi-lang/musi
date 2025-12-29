mod lexer;

use proc_macro::TokenStream;

#[proc_macro_derive(Lexer, attributes(lexer, token, regex, callback))]
pub fn derive_lexer(input: TokenStream) -> TokenStream {
    lexer::derive_lexer_impl(input.into())
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}
