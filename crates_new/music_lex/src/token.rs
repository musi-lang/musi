use music_basic::Span;
use smallvec::SmallVec;
use std::fmt::{Display, Formatter, Result as FmtResult};

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
    Hash,
    Amp,
    Caret,
    Tilde,

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

#[must_use]
pub const fn display_token_kind(kind: &TokenKind) -> TokenKindDisplay<'_> {
    TokenKindDisplay(kind)
}

pub struct TokenKindDisplay<'a>(&'a TokenKind);

impl Display for TokenKindDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self.0 {
            TokenKind::IntLit => write!(f, "integer literal"),
            TokenKind::FloatLit => write!(f, "float literal"),
            TokenKind::StringLit => write!(f, "string literal"),
            TokenKind::FStringLit(_) => write!(f, "formatted string literal"),
            TokenKind::RuneLit => write!(f, "rune literal"),
            TokenKind::Ident => write!(f, "identifier"),
            TokenKind::EscapedIdent => write!(f, "escaped identifier"),
            TokenKind::KwAnd => write!(f, "'and'"),
            TokenKind::KwAs => write!(f, "'as'"),
            TokenKind::KwCase => write!(f, "'case'"),
            TokenKind::KwClass => write!(f, "'class'"),
            TokenKind::KwData => write!(f, "'data'"),
            TokenKind::KwEffect => write!(f, "'effect'"),
            TokenKind::KwExport => write!(f, "'export'"),
            TokenKind::KwForeign => write!(f, "'foreign'"),
            TokenKind::KwHandle => write!(f, "'handle'"),
            TokenKind::KwIf => write!(f, "'if'"),
            TokenKind::KwImport => write!(f, "'import'"),
            TokenKind::KwIn => write!(f, "'in'"),
            TokenKind::KwInstance => write!(f, "'instance'"),
            TokenKind::KwLaw => write!(f, "'law'"),
            TokenKind::KwLet => write!(f, "'let'"),
            TokenKind::KwMut => write!(f, "'mut'"),
            TokenKind::KwNot => write!(f, "'not'"),
            TokenKind::KwOf => write!(f, "'of'"),
            TokenKind::KwOpaque => write!(f, "'opaque'"),
            TokenKind::KwOr => write!(f, "'or'"),
            TokenKind::KwPerform => write!(f, "'perform'"),
            TokenKind::KwQuote => write!(f, "'quote'"),
            TokenKind::KwResume => write!(f, "'resume'"),
            TokenKind::KwShl => write!(f, "'shl'"),
            TokenKind::KwShr => write!(f, "'shr'"),
            TokenKind::KwWhere => write!(f, "'where'"),
            TokenKind::KwWith => write!(f, "'with'"),
            TokenKind::KwXor => write!(f, "'xor'"),
            TokenKind::LParen => write!(f, "'('"),
            TokenKind::RParen => write!(f, "')'"),
            TokenKind::LBracket => write!(f, "'['"),
            TokenKind::RBracket => write!(f, "']'"),
            TokenKind::LBrace => write!(f, "'{{'"),
            TokenKind::RBrace => write!(f, "'}}'"),
            TokenKind::Semi => write!(f, "';'"),
            TokenKind::Comma => write!(f, "','"),
            TokenKind::Dot => write!(f, "'.'"),
            TokenKind::Colon => write!(f, "':'"),
            TokenKind::Pipe => write!(f, "'|'"),
            TokenKind::Bang => write!(f, "'!'"),
            TokenKind::Question => write!(f, "'?'"),
            TokenKind::Lt => write!(f, "'<'"),
            TokenKind::Gt => write!(f, "'>'"),
            TokenKind::Eq => write!(f, "'='"),
            TokenKind::Plus => write!(f, "'+'"),
            TokenKind::Minus => write!(f, "'-'"),
            TokenKind::Star => write!(f, "'*'"),
            TokenKind::Slash => write!(f, "'/'"),
            TokenKind::Percent => write!(f, "'%'"),
            TokenKind::At => write!(f, "'@'"),
            TokenKind::Hash => write!(f, "'#'"),
            TokenKind::Amp => write!(f, "'&'"),
            TokenKind::Caret => write!(f, "'^'"),
            TokenKind::Tilde => write!(f, "'~'"),
            TokenKind::ColonEq => write!(f, "':='"),
            TokenKind::LtMinus => write!(f, "'<-'"),
            TokenKind::MinusGt => write!(f, "'->'"),
            TokenKind::TildeGt => write!(f, "'~>'"),
            TokenKind::EqGt => write!(f, "'=>'"),
            TokenKind::SlashEq => write!(f, "'/='"),
            TokenKind::LtEq => write!(f, "'<='"),
            TokenKind::GtEq => write!(f, "'>='"),
            TokenKind::DotDotDot => write!(f, "'...'"),
            TokenKind::DotLBracket => write!(f, "'.['"),
            TokenKind::DotLBrace => write!(f, "'.{{'"),
            TokenKind::ColonQuestion => write!(f, "':?'"),
            TokenKind::ColonQuestionGt => write!(f, "':?>'"),
            TokenKind::LtColon => write!(f, "'<:'"),
            TokenKind::PipeGt => write!(f, "'|>'"),
            TokenKind::QuestionDot => write!(f, "'?.'"),
            TokenKind::BangDot => write!(f, "'!.'"),
            TokenKind::SpliceLParen => write!(f, "'#('"),
            TokenKind::SpliceLBracket => write!(f, "'#['"),
            TokenKind::Eof => write!(f, "end of file"),
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
