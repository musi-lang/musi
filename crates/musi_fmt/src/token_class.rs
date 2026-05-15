use music_syntax::TokenKind;

pub fn is_operator(kind: TokenKind) -> bool {
    const OPERATORS: &[TokenKind] = &[
        TokenKind::ColonEq,
        TokenKind::MinusGt,
        TokenKind::TildeEq,
        TokenKind::EqGt,
        TokenKind::SlashEq,
        TokenKind::LtEq,
        TokenKind::GtEq,
        TokenKind::PipeGt,
        TokenKind::Pipe,
        TokenKind::Plus,
        TokenKind::Minus,
        TokenKind::Star,
        TokenKind::Slash,
        TokenKind::Percent,
        TokenKind::Eq,
        TokenKind::Lt,
        TokenKind::Gt,
        TokenKind::KwAnd,
        TokenKind::KwOr,
        TokenKind::KwIn,
        TokenKind::KwXor,
    ];
    OPERATORS.contains(&kind)
}

pub const fn is_word_like(kind: TokenKind) -> bool {
    is_name_like(kind) || is_literal_like(kind) || is_spacing_keyword(kind)
}

const fn is_name_like(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Ident | TokenKind::OpIdent | TokenKind::Underscore
    )
}

const fn is_literal_like(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Int
            | TokenKind::Float
            | TokenKind::String
            | TokenKind::Rune
            | TokenKind::TemplateNoSubst
            | TokenKind::TemplateHead
            | TokenKind::TemplateMiddle
            | TokenKind::TemplateTail
    )
}

const fn is_spacing_keyword(kind: TokenKind) -> bool {
    is_declaration_keyword(kind)
        || is_control_keyword(kind)
        || is_modifier_keyword(kind)
        || is_other_spacing_keyword(kind)
}

const fn is_declaration_keyword(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::KwShape | TokenKind::KwData | TokenKind::KwLet
    )
}

const fn is_control_keyword(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::KwDefer
            | TokenKind::KwIf
            | TokenKind::KwElse
            | TokenKind::KwMatch
            | TokenKind::KwThen
            | TokenKind::KwYield
    )
}

const fn is_modifier_keyword(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::KwKnown
            | TokenKind::KwExport
            | TokenKind::KwHidden
            | TokenKind::KwMut
            | TokenKind::KwRecur
            | TokenKind::KwUnsafe
    )
}

const fn is_other_spacing_keyword(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::KwAs
            | TokenKind::KwErased
            | TokenKind::KwImport
            | TokenKind::KwNot
            | TokenKind::KwWhere
    )
}
