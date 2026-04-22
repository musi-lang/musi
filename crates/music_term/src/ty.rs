use std::fmt::{self, Display, Formatter};

use serde::{Deserialize, Serialize};
use thiserror::Error;

pub type TypeTermResult<T = ()> = Result<T, TypeTermError>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeTerm {
    pub kind: TypeTermKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TypeTermKind {
    Error,
    Unknown,
    Type,
    Syntax,
    Any,
    Empty,
    Unit,
    Bool,
    Nat,
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Nat8,
    Nat16,
    Nat32,
    Nat64,
    Float,
    Float32,
    Float64,
    String,
    Rune,
    CString,
    CPtr,
    NatLit(u64),
    Named {
        module: Option<TypeModuleRef>,
        name: Box<str>,
        args: Box<[TypeTerm]>,
    },
    Pi {
        binder: Box<str>,
        binder_ty: Box<TypeTerm>,
        body: Box<TypeTerm>,
        is_effectful: bool,
    },
    Arrow {
        params: Box<[TypeTerm]>,
        ret: Box<TypeTerm>,
        is_effectful: bool,
    },
    Sum {
        left: Box<TypeTerm>,
        right: Box<TypeTerm>,
    },
    Tuple {
        items: Box<[TypeTerm]>,
    },
    Seq {
        item: Box<TypeTerm>,
    },
    Array {
        dims: Box<[TypeDim]>,
        item: Box<TypeTerm>,
    },
    Range {
        bound: Box<TypeTerm>,
    },
    Handler {
        effect: Box<TypeTerm>,
        input: Box<TypeTerm>,
        output: Box<TypeTerm>,
    },
    Mut {
        inner: Box<TypeTerm>,
    },
    AnyShape {
        capability: Box<TypeTerm>,
    },
    SomeShape {
        capability: Box<TypeTerm>,
    },
    Record {
        fields: Box<[TypeField]>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TypeTermSugarKind {
    Optional,
    Fallible,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeTermSugar {
    pub kind: TypeTermSugarKind,
    pub value: Box<TypeTerm>,
    pub error: Option<Box<TypeTerm>>,
}

impl TypeTermSugar {
    #[must_use]
    pub fn optional(value: TypeTerm) -> Self {
        Self {
            kind: TypeTermSugarKind::Optional,
            value: Box::new(value),
            error: None,
        }
    }

    #[must_use]
    pub fn fallible(error: TypeTerm, value: TypeTerm) -> Self {
        Self {
            kind: TypeTermSugarKind::Fallible,
            value: Box::new(value),
            error: Some(Box::new(error)),
        }
    }
}

struct SimpleTypeTermInfo {
    kind: TypeTermKind,
    parse_name: &'static str,
    display_name: &'static str,
}

const SIMPLE_TYPE_TERMS: &[SimpleTypeTermInfo] = &[
    SimpleTypeTermInfo {
        kind: TypeTermKind::Error,
        parse_name: "Error",
        display_name: "<error>",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Unknown,
        parse_name: "Unknown",
        display_name: "Unknown",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Type,
        parse_name: "Type",
        display_name: "Type",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Syntax,
        parse_name: "Syntax",
        display_name: "Syntax",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Any,
        parse_name: "Any",
        display_name: "Any",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Empty,
        parse_name: "Empty",
        display_name: "Empty",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Unit,
        parse_name: "Unit",
        display_name: "Unit",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Bool,
        parse_name: "Bool",
        display_name: "Bool",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Nat,
        parse_name: "Nat",
        display_name: "Nat",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Int,
        parse_name: "Int",
        display_name: "Int",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Int8,
        parse_name: "Int8",
        display_name: "Int8",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Int16,
        parse_name: "Int16",
        display_name: "Int16",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Int32,
        parse_name: "Int32",
        display_name: "Int32",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Int64,
        parse_name: "Int64",
        display_name: "Int64",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Nat8,
        parse_name: "Nat8",
        display_name: "Nat8",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Nat16,
        parse_name: "Nat16",
        display_name: "Nat16",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Nat32,
        parse_name: "Nat32",
        display_name: "Nat32",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Nat64,
        parse_name: "Nat64",
        display_name: "Nat64",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Float,
        parse_name: "Float",
        display_name: "Float",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Float32,
        parse_name: "Float32",
        display_name: "Float32",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Float64,
        parse_name: "Float64",
        display_name: "Float64",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::String,
        parse_name: "String",
        display_name: "String",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::Rune,
        parse_name: "Rune",
        display_name: "Rune",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::CString,
        parse_name: "CString",
        display_name: "CString",
    },
    SimpleTypeTermInfo {
        kind: TypeTermKind::CPtr,
        parse_name: "CPtr",
        display_name: "CPtr",
    },
];

fn simple_type_term_info(kind: &TypeTermKind) -> Option<&'static SimpleTypeTermInfo> {
    SIMPLE_TYPE_TERMS.iter().find(|term| &term.kind == kind)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeModuleRef {
    pub spec: Box<str>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TypeDim {
    Unknown,
    Name(Box<str>),
    Int(u32),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeField {
    pub name: Box<str>,
    pub ty: TypeTerm,
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum TypeTermError {
    #[error("type term parse failed")]
    TermParseFailed,
}

impl TypeTerm {
    #[must_use]
    pub const fn new(kind: TypeTermKind) -> Self {
        Self { kind }
    }

    #[must_use]
    /// # Panics
    ///
    /// Panics if serializing the already-validated type-term structure fails.
    pub fn to_json(&self) -> String {
        serde_json::to_string(self).expect("type term serialization should succeed")
    }

    /// # Errors
    ///
    /// Returns [`TypeTermError`] when `text` is not valid type-term JSON.
    pub fn from_json(text: &str) -> Result<Self, TypeTermError> {
        serde_json::from_str(text).map_err(|_| TypeTermError::TermParseFailed)
    }
}

impl Display for TypeTerm {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(result) = fmt_atomic_type_term_kind(f, &self.kind) {
            return result;
        }
        match &self.kind {
            TypeTermKind::Named { name, args, .. } => fmt_named_type_term(f, name, args),
            TypeTermKind::Pi {
                binder,
                binder_ty,
                body,
                is_effectful,
            } => fmt_pi_type_term(f, binder, binder_ty, body, *is_effectful),
            TypeTermKind::Arrow {
                params,
                ret,
                is_effectful,
            } => fmt_arrow_type_term(f, params, ret, *is_effectful),
            TypeTermKind::Sum { left, right } => write!(f, "{left} + {right}"),
            TypeTermKind::Tuple { items } => fmt_tuple_type_term(f, items),
            TypeTermKind::Seq { item } => write!(f, "[]{item}"),
            TypeTermKind::Array { dims, item } => fmt_array_type_term(f, dims, item),
            TypeTermKind::Range { bound } => fmt_applied_name(f, "Range", bound),
            TypeTermKind::Handler {
                effect,
                input,
                output,
            } => write!(f, "answer {effect} ({input} -> {output})"),
            TypeTermKind::Mut { inner } => write!(f, "mut {inner}"),
            TypeTermKind::AnyShape { capability } => write!(f, "any {capability}"),
            TypeTermKind::SomeShape { capability } => write!(f, "some {capability}"),
            TypeTermKind::Record { fields } => fmt_record_type_term(f, fields),
            TypeTermKind::Error
            | TypeTermKind::Unknown
            | TypeTermKind::Type
            | TypeTermKind::Syntax
            | TypeTermKind::Any
            | TypeTermKind::Empty
            | TypeTermKind::Unit
            | TypeTermKind::Bool
            | TypeTermKind::Nat
            | TypeTermKind::Int
            | TypeTermKind::Int8
            | TypeTermKind::Int16
            | TypeTermKind::Int32
            | TypeTermKind::Int64
            | TypeTermKind::Nat8
            | TypeTermKind::Nat16
            | TypeTermKind::Nat32
            | TypeTermKind::Nat64
            | TypeTermKind::Float
            | TypeTermKind::Float32
            | TypeTermKind::Float64
            | TypeTermKind::String
            | TypeTermKind::Rune
            | TypeTermKind::CString
            | TypeTermKind::CPtr
            | TypeTermKind::NatLit(_) => {
                fmt_atomic_type_term_kind(f, &self.kind).unwrap_or(Err(fmt::Error))
            }
        }
    }
}

fn fmt_atomic_type_term_kind(f: &mut Formatter<'_>, kind: &TypeTermKind) -> Option<fmt::Result> {
    if let TypeTermKind::NatLit(value) = kind {
        return Some(write!(f, "{value}"));
    }
    simple_type_term_info(kind).map(|term| f.write_str(term.display_name))
}

fn fmt_named_type_term(f: &mut Formatter<'_>, name: &str, args: &[TypeTerm]) -> fmt::Result {
    if args.is_empty() {
        f.write_str(name)
    } else {
        write!(
            f,
            "{}[{}]",
            name,
            args.iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

fn fmt_pi_type_term(
    f: &mut Formatter<'_>,
    binder: &str,
    binder_ty: &TypeTerm,
    body: &TypeTerm,
    is_effectful: bool,
) -> fmt::Result {
    write!(
        f,
        "forall ({binder} : {binder_ty}) {} {body}",
        if is_effectful { "~>" } else { "->" }
    )
}

fn fmt_arrow_type_term(
    f: &mut Formatter<'_>,
    params: &[TypeTerm],
    ret: &TypeTerm,
    is_effectful: bool,
) -> fmt::Result {
    let params = params.iter().map(ToString::to_string).collect::<Vec<_>>();
    let left = if params.len() == 1 {
        params[0].clone()
    } else {
        format!("({})", params.join(", "))
    };
    write!(f, "{left} {} {ret}", if is_effectful { "~>" } else { "->" })
}

fn fmt_tuple_type_term(f: &mut Formatter<'_>, items: &[TypeTerm]) -> fmt::Result {
    write!(
        f,
        "({})",
        items
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn fmt_array_type_term(f: &mut Formatter<'_>, dims: &[TypeDim], item: &TypeTerm) -> fmt::Result {
    write!(
        f,
        "[{}]{item}",
        dims.iter()
            .map(|dim| match dim {
                TypeDim::Unknown => "_".into(),
                TypeDim::Name(name) => name.to_string(),
                TypeDim::Int(value) => value.to_string(),
            })
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn fmt_record_type_term(f: &mut Formatter<'_>, fields: &[TypeField]) -> fmt::Result {
    write!(
        f,
        "{{{}}}",
        fields
            .iter()
            .map(|field| format!("{} = {}", field.name, field.ty))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn fmt_applied_name(f: &mut Formatter<'_>, name: &str, bound: &TypeTerm) -> fmt::Result {
    write!(f, "{name}[{bound}]")
}

/// # Errors
///
/// Returns [`TypeTermError`] when `text` is not valid textual type-term syntax.
pub fn parse_type_term(text: &str) -> TypeTermResult<TypeTerm> {
    Parser::new(text).parse()
}

struct Parser<'a> {
    text: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    const fn new(text: &'a str) -> Self {
        Self { text, pos: 0 }
    }

    fn parse(mut self) -> TypeTermResult<TypeTerm> {
        let term = self.parse_sum()?;
        self.skip_ws();
        if self.pos == self.text.len() {
            Ok(term)
        } else {
            Err(TypeTermError::TermParseFailed)
        }
    }

    fn parse_sum(&mut self) -> TypeTermResult<TypeTerm> {
        let mut left = self.parse_arrow()?;
        loop {
            self.skip_ws();
            if !self.consume("+") {
                return Ok(left);
            }
            let right = self.parse_arrow()?;
            left = TypeTerm::new(TypeTermKind::Sum {
                left: Box::new(left),
                right: Box::new(right),
            });
        }
    }

    fn parse_arrow(&mut self) -> TypeTermResult<TypeTerm> {
        let left = self.parse_prefix()?;
        self.skip_ws();
        if self.consume("->") || self.consume("~>") {
            let is_effectful = self
                .text
                .get(self.pos.saturating_sub(2)..self.pos)
                .is_some_and(|token| token == "~>");
            let right = self.parse_arrow()?;
            let params = match left.kind {
                TypeTermKind::Tuple { items } => items,
                _ => vec![left].into_boxed_slice(),
            };
            return Ok(TypeTerm::new(TypeTermKind::Arrow {
                params,
                ret: Box::new(right),
                is_effectful,
            }));
        }
        Ok(left)
    }
}

impl Parser<'_> {
    fn parse_prefix(&mut self) -> TypeTermResult<TypeTerm> {
        if let Some(term) = self.parse_mut_prefix()? {
            return Ok(term);
        }
        if let Some(term) = self.parse_existential_prefix()? {
            return Ok(term);
        }
        if let Some(term) = self.parse_handler_prefix()? {
            return Ok(term);
        }
        if let Some(term) = self.parse_pi_prefix()? {
            return Ok(term);
        }
        self.parse_atom()
    }
}

impl Parser<'_> {
    fn parse_atom(&mut self) -> TypeTermResult<TypeTerm> {
        self.skip_ws();
        if self.consume("(") {
            return self.parse_tuple_atom();
        }
        if self.consume("{") {
            return self.parse_record_atom();
        }
        if self.consume("[") {
            return self.parse_array_atom();
        }
        if let Some(value) = self.parse_nat_lit() {
            return Ok(TypeTerm::new(TypeTermKind::NatLit(value)));
        }
        self.parse_named_or_simple_type_atom()
    }

    fn parse_named_or_simple_type_atom(&mut self) -> TypeTermResult<TypeTerm> {
        let name = self.parse_ident()?;
        if let Some(kind) = simple_type_kind(&name) {
            return Ok(TypeTerm::new(kind));
        }
        let mut args = Vec::new();
        self.skip_ws();
        if self.consume("[") {
            self.skip_ws();
            if !self.consume("]") {
                loop {
                    args.push(self.parse_sum()?);
                    self.skip_ws();
                    if self.consume("]") {
                        break;
                    }
                    self.expect(",")?;
                }
            }
        }
        Ok(TypeTerm::new(TypeTermKind::Named {
            module: None,
            name: name.into(),
            args: args.into_boxed_slice(),
        }))
    }
}

impl Parser<'_> {
    fn parse_mut_prefix(&mut self) -> TypeTermResult<Option<TypeTerm>> {
        self.skip_ws();
        if !self.consume("mut") {
            return Ok(None);
        }
        self.require_ws()?;
        let inner = self.parse_prefix()?;
        Ok(Some(TypeTerm::new(TypeTermKind::Mut {
            inner: Box::new(inner),
        })))
    }

    fn parse_existential_prefix(&mut self) -> TypeTermResult<Option<TypeTerm>> {
        self.skip_ws();
        let kind = if self.consume("any") {
            Some("any")
        } else if self.consume("some") {
            Some("some")
        } else {
            None
        };
        let Some(kind) = kind else {
            return Ok(None);
        };
        self.require_ws()?;
        let shape = Box::new(self.parse_prefix()?);
        let term_kind = match kind {
            "any" => TypeTermKind::AnyShape { capability: shape },
            _ => TypeTermKind::SomeShape { capability: shape },
        };
        Ok(Some(TypeTerm::new(term_kind)))
    }

    fn parse_handler_prefix(&mut self) -> TypeTermResult<Option<TypeTerm>> {
        self.skip_ws();
        if !self.consume("answer") {
            return Ok(None);
        }
        self.require_ws()?;
        let effect = self.parse_sum()?;
        self.expect("(")?;
        let input = self.parse_sum()?;
        self.skip_ws();
        self.expect("->")?;
        let output = self.parse_sum()?;
        self.expect(")")?;
        Ok(Some(TypeTerm::new(TypeTermKind::Handler {
            effect: Box::new(effect),
            input: Box::new(input),
            output: Box::new(output),
        })))
    }

    fn parse_pi_prefix(&mut self) -> TypeTermResult<Option<TypeTerm>> {
        self.skip_ws();
        if !self.consume("forall") {
            return Ok(None);
        }
        self.require_ws()?;
        self.expect("(")?;
        let binder = self.parse_ident()?;
        self.skip_ws();
        self.expect(":")?;
        let binder_ty = self.parse_sum()?;
        self.expect(")")?;
        let is_effectful = self.consume_effect_arrow()?;
        let body = self.parse_sum()?;
        Ok(Some(TypeTerm::new(TypeTermKind::Pi {
            binder: binder.into(),
            binder_ty: Box::new(binder_ty),
            body: Box::new(body),
            is_effectful,
        })))
    }

    fn consume_effect_arrow(&mut self) -> TypeTermResult<bool> {
        self.skip_ws();
        if self.consume("~>") {
            Ok(true)
        } else {
            self.expect("->")?;
            Ok(false)
        }
    }
}

impl Parser<'_> {
    fn parse_tuple_atom(&mut self) -> TypeTermResult<TypeTerm> {
        let mut items = Vec::new();
        loop {
            items.push(self.parse_sum()?);
            self.skip_ws();
            if self.consume(")") {
                break;
            }
            self.expect(",")?;
        }
        Ok(TypeTerm::new(TypeTermKind::Tuple {
            items: items.into_boxed_slice(),
        }))
    }

    fn parse_record_atom(&mut self) -> TypeTermResult<TypeTerm> {
        let mut fields = Vec::new();
        self.skip_ws();
        if self.consume("}") {
            return Ok(TypeTerm::new(TypeTermKind::Record {
                fields: fields.into_boxed_slice(),
            }));
        }
        loop {
            let name = self.parse_ident()?;
            self.skip_ws();
            self.expect("=")?;
            let ty = self.parse_sum()?;
            fields.push(TypeField {
                name: name.into(),
                ty,
            });
            self.skip_ws();
            if self.consume("}") {
                break;
            }
            self.expect(",")?;
        }
        Ok(TypeTerm::new(TypeTermKind::Record {
            fields: fields.into_boxed_slice(),
        }))
    }

    fn parse_array_atom(&mut self) -> TypeTermResult<TypeTerm> {
        let mut dims = Vec::new();
        self.skip_ws();
        if !self.consume("]") {
            loop {
                dims.push(self.parse_dim()?);
                self.skip_ws();
                if self.consume("]") {
                    break;
                }
                self.expect(",")?;
            }
        }
        let item = self.parse_prefix()?;
        if dims.is_empty() {
            Ok(TypeTerm::new(TypeTermKind::Seq {
                item: Box::new(item),
            }))
        } else {
            Ok(TypeTerm::new(TypeTermKind::Array {
                dims: dims.into_boxed_slice(),
                item: Box::new(item),
            }))
        }
    }

    fn parse_dim(&mut self) -> TypeTermResult<TypeDim> {
        self.skip_ws();
        if self.consume("_") {
            return Ok(TypeDim::Unknown);
        }
        if let Some(value) = self.parse_nat_lit() {
            return Ok(TypeDim::Int(
                u32::try_from(value).map_err(|_| TypeTermError::TermParseFailed)?,
            ));
        }
        Ok(TypeDim::Name(self.parse_ident()?.into()))
    }
}

impl<'a> Parser<'a> {
    fn parse_nat_lit(&mut self) -> Option<u64> {
        self.skip_ws();
        let start = self.pos;
        while self.peek().is_some_and(|ch| ch.is_ascii_digit()) {
            self.bump();
        }
        self.slice(start, self.pos)
            .filter(|_| self.pos > start)
            .and_then(|digits| digits.parse().ok())
    }

    fn parse_ident(&mut self) -> TypeTermResult<String> {
        self.skip_ws();
        let start = self.pos;
        while self
            .peek()
            .is_some_and(|ch| ch.is_alphanumeric() || matches!(ch, '_' | ':' | '.'))
        {
            self.bump();
        }
        if self.pos == start {
            return Err(TypeTermError::TermParseFailed);
        }
        self.slice(start, self.pos)
            .map(ToOwned::to_owned)
            .ok_or(TypeTermError::TermParseFailed)
    }

    fn require_ws(&mut self) -> TypeTermResult {
        let before = self.pos;
        self.skip_ws();
        if self.pos == before {
            Err(TypeTermError::TermParseFailed)
        } else {
            Ok(())
        }
    }

    fn expect(&mut self, token: &str) -> TypeTermResult {
        if self.consume(token) {
            Ok(())
        } else {
            Err(TypeTermError::TermParseFailed)
        }
    }

    fn consume(&mut self, token: &str) -> bool {
        self.skip_ws();
        if self
            .remaining_text()
            .is_some_and(|rest| rest.starts_with(token))
        {
            self.pos += token.len();
            true
        } else {
            false
        }
    }

    fn skip_ws(&mut self) {
        while self.peek().is_some_and(char::is_whitespace) {
            self.bump();
        }
    }

    fn peek(&self) -> Option<char> {
        self.remaining_text()?.chars().next()
    }

    fn bump(&mut self) {
        if let Some(ch) = self.peek() {
            self.pos += ch.len_utf8();
        }
    }

    fn remaining_text(&self) -> Option<&'a str> {
        self.text.get(self.pos..)
    }

    fn slice(&self, start: usize, end: usize) -> Option<&'a str> {
        self.text.get(start..end)
    }
}

fn simple_type_kind(name: &str) -> Option<TypeTermKind> {
    SIMPLE_TYPE_TERMS
        .iter()
        .find_map(|term| (term.parse_name == name).then(|| term.kind.clone()))
}
