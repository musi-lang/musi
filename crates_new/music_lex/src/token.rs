use music_basic::Span;
use smallvec::SmallVec;

pub type TokenKinds = Vec<TokenKind>;
pub type Trivias = SmallVec<[Trivia; 2]>;
pub type FStringParts = SmallVec<[FStringPart; 4]>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub leading_trivia: Trivias,
    pub trailing_trivia: Trivias,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trivia {
    pub kind: TriviaKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TriviaKind {
    Whitespace,
    LineComment { doc: bool },
    BlockComment { doc: bool },
    Newline,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FStringPartKind {
    Literal,
    Interpolation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FStringPart {
    pub kind: FStringPartKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    IntLit,
    FloatLit,
    StringLit,
    FStringLit(FStringParts),
    RuneLit,

    Ident,
    EscapedIdent,

    KwAnd,
    KwAs,
    KwCase,
    KwClass,
    KwData,
    KwEffect,
    KwExport,
    KwForeign,
    KwHandle,
    KwIf,
    KwImport,
    KwIn,
    KwInstance,
    KwLaw,
    KwLet,
    KwMut,
    KwNot,
    KwOf,
    KwOpaque,
    KwOr,
    KwPerform,
    KwQuote,
    KwResume,
    KwShl,
    KwShr,
    KwWhere,
    KwWith,
    KwXor,

    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    Semi,
    Comma,
    Dot,
    Colon,
    Pipe,
    Bang,
    Question,
    Lt,
    Gt,
    Eq,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    At,

    ColonEq,
    LtMinus,
    MinusGt,
    TildeGt,
    EqGt,
    SlashEq,
    LtEq,
    GtEq,
    DotDotDot,
    DotLBracket,
    DotLBrace,
    ColonQuestion,
    ColonQuestionGt,
    LtColon,
    PipeGt,
    QuestionDot,
    BangDot,
    SpliceLParen,
    SpliceLBracket,

    Eof,
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
