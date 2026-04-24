use music_syntax::TokenKind;

pub fn is_operator(kind: TokenKind) -> bool {
    const OPERATORS: &[TokenKind] = &[
        TokenKind::ColonEq,
        TokenKind::MinusGt,
        TokenKind::TildeGt,
        TokenKind::TildeEq,
        TokenKind::EqGt,
        TokenKind::SlashEq,
        TokenKind::LtEq,
        TokenKind::GtEq,
        TokenKind::LtColon,
        TokenKind::ColonQuestion,
        TokenKind::ColonQuestionGt,
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
        TokenKind::KwShl,
        TokenKind::KwShr,
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
        TokenKind::KwShape
            | TokenKind::KwData
            | TokenKind::KwEffect
            | TokenKind::KwGiven
            | TokenKind::KwLaw
            | TokenKind::KwLet
    )
}

const fn is_control_keyword(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::KwHandle
            | TokenKind::KwIf
            | TokenKind::KwMatch
            | TokenKind::KwAsk
            | TokenKind::KwResume
    )
}

const fn is_modifier_keyword(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::KwComptime
            | TokenKind::KwExport
            | TokenKind::KwNative
            | TokenKind::KwMut
            | TokenKind::KwOpaque
            | TokenKind::KwPartial
            | TokenKind::KwRec
            | TokenKind::KwUnsafe
    )
}

const fn is_other_spacing_keyword(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::KwAnswer
            | TokenKind::KwAny
            | TokenKind::KwAs
            | TokenKind::KwImport
            | TokenKind::KwNot
            | TokenKind::KwRequire
            | TokenKind::KwQuote
            | TokenKind::KwSome
            | TokenKind::KwWhere
    )
}
