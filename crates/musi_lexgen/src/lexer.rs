use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    Attribute, Data, DeriveInput, Expr, ExprLit, Ident, Lit, MetaNameValue, Result, Variant, parse2,
};

/// Building logic for Lexer macro.
pub fn derive_lexer_impl(input: TokenStream) -> Result<TokenStream> {
    let input: DeriveInput = parse2(input)?;
    let enum_name = &input.ident;

    let variants = match &input.data {
        Data::Enum(data_enum) => &data_enum.variants,
        _ => {
            return Err(syn::Error::new_spanned(
                input,
                "Lexer can only be derived for enums",
            ));
        }
    };

    let skip_patterns = extract_skip_patterns(&input.attrs)?;
    let mut token_defs = vec![];

    for variant in variants {
        if let Some(def) = parse_token_variant(variant)? {
            token_defs.push(def);
        }
    }

    token_defs.sort_by(|a, b| b.pattern.len().cmp(&a.pattern.len()));

    let generated = generate_lexer_impl(enum_name, &skip_patterns, &token_defs);
    Ok(generated)
}

struct TokenDef {
    variant_name: Ident,
    pattern: String,
    is_keyword: bool,
}

fn extract_skip_patterns(attrs: &[Attribute]) -> Result<Vec<String>> {
    let mut patterns = vec![];
    for attr in attrs {
        if !attr.path().is_ident("lexer") {
            continue;
        }
        let nested: MetaNameValue = attr.parse_args()?;
        if nested.path.is_ident("skip")
            && let Expr::Lit(ExprLit {
                lit: Lit::Str(lit_str),
                ..
            }) = &nested.value
        {
            patterns.push(lit_str.value());
        }
    }
    Ok(patterns)
}

fn parse_token_variant(variant: &Variant) -> Result<Option<TokenDef>> {
    for attr in &variant.attrs {
        if attr.path().is_ident("token") {
            let lit_str: syn::LitStr = attr.parse_args()?;
            let pattern = lit_str.value();
            let is_keyword = pattern.chars().all(|c| c.is_ascii_alphabetic() || c == '_');
            return Ok(Some(TokenDef {
                variant_name: variant.ident.clone(),
                pattern,
                is_keyword,
            }));
        }
    }
    Ok(None)
}

fn generate_lexer_impl(
    enum_name: &Ident,
    skip_patterns: &[String],
    token_defs: &[TokenDef],
) -> TokenStream {
    let symbol_arms = generate_symbol_dispatch(enum_name, token_defs);
    let keyword_arms = generate_keyword_lookup(enum_name, token_defs);
    let skip_check = generate_skip_check(skip_patterns);

    let lexer_struct_name = format_ident!("{enum_name}Lexer");

    quote! {
        impl #enum_name {
            /// Starts lexer for input text.
            #[must_use]
            pub fn lexer(input: &str) -> #lexer_struct_name<'_> {
                #lexer_struct_name::new(input)
            }

            /// Checks if word is keyword.
            #[must_use]
            pub fn keyword_lookup(ident: &str) -> Option<Self> {
                match ident {
                    #(#keyword_arms)*
                    _ => None,
                }
            }

            /// Matches symbols. Prefers longest matches.
            #[must_use]
            pub fn symbol_dispatch(
                first: char,
                peek_nth: impl Fn(usize) -> Option<char>,
            ) -> Option<(Self, usize)> {
                #(#symbol_arms)*
                None
            }
        }

        /// Tracking state for lexer.
        pub struct #lexer_struct_name<'a> {
            input: &'a str,
            pos: usize,
        }

        impl<'a> #lexer_struct_name<'a> {
            /// Starts fresh lexer.
            #[must_use]
            pub const fn new(input: &'a str) -> Self {
                Self { input, pos: 0 }
            }

            /// Gives current byte position.
            #[must_use]
            pub const fn pos(&self) -> usize {
                self.pos
            }

            /// Gives text between two positions.
            #[must_use]
            pub fn slice(&self, start: usize, end: usize) -> &'a str {
                self.input.get(start..end).unwrap_or("")
            }

            fn skip_trivia(&mut self) {
                #skip_check
            }

            fn peek(&self) -> Option<char> {
                self.input.get(self.pos..)?.chars().next()
            }

            fn peek_nth(&self, n: usize) -> Option<char> {
                self.input.get(self.pos..)?.chars().nth(n)
            }

            fn bump(&mut self) -> Option<char> {
                let ch = self.peek()?;
                self.pos += ch.len_utf8();
                Some(ch)
            }

            fn bump_n(&mut self, n: usize) {
                for _ in 0..n {
                    let _ = self.bump();
                }
            }
        }

        impl Iterator for #lexer_struct_name<'_> {
            type Item = (Option<#enum_name>, usize, usize);

            fn next(&mut self) -> Option<Self::Item> {
                self.skip_trivia();
                let start = self.pos;

                let first_char = self.peek()?;

                if let Some((token, len)) = #enum_name::symbol_dispatch(
                    first_char,
                    |n| self.peek_nth(n + 1),
                ) {
                    self.bump_n(len);
                    return Some((Some(token), start, self.pos));
                }

                if first_char.is_ascii_alphanumeric() || first_char == '_' {
                    while self.peek().is_some_and(|c| c.is_ascii_alphanumeric() || c == '_') {
                        let _ = self.bump();
                    }
                    let ident = &self.input[start..self.pos];
                    let token = #enum_name::keyword_lookup(ident);
                    return Some((token, start, self.pos));
                }

                let _ = self.bump();
                Some((None, start, self.pos))
            }
        }
    }
}

fn generate_symbol_dispatch(enum_name: &Ident, token_defs: &[TokenDef]) -> Vec<TokenStream> {
    token_defs
        .iter()
        .filter(|def| !def.is_keyword)
        .filter_map(|def| {
            let variant = &def.variant_name;
            let chars: Vec<char> = def.pattern.chars().collect();
            let first_char = *chars.first()?;
            let pattern_len = chars.len();

            let rest_checks: Vec<TokenStream> = chars
                .iter()
                .skip(1)
                .enumerate()
                .map(|(i, c)| quote! { peek_nth(#i) == Some(#c) })
                .collect();

            Some(if rest_checks.is_empty() {
                quote! {
                    if first == #first_char {
                        return Some((#enum_name::#variant, 1));
                    }
                }
            } else {
                quote! {
                    if first == #first_char #(&& #rest_checks)* {
                        return Some((#enum_name::#variant, #pattern_len));
                    }
                }
            })
        })
        .collect()
}

fn generate_keyword_lookup(enum_name: &Ident, token_defs: &[TokenDef]) -> Vec<TokenStream> {
    token_defs
        .iter()
        .filter(|def| def.is_keyword)
        .map(|def| {
            let pattern = &def.pattern;
            let variant = &def.variant_name;
            quote! { #pattern => Some(#enum_name::#variant), }
        })
        .collect()
}

fn generate_skip_check(skip_patterns: &[String]) -> TokenStream {
    if skip_patterns.is_empty() {
        return quote! {};
    }

    quote! {
        loop {
            let before = self.pos;
            while self.peek().is_some_and(|c| c.is_ascii_whitespace()) {
                let _ = self.bump();
            }
            if self.pos == before {
                break;
            }
        }
    }
}
