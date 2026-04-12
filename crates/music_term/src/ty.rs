use std::fmt::{self, Display, Formatter};

use serde::{Deserialize, Serialize};
use thiserror::Error;

pub type TypeTermResult<T = ()> = Result<T, TypeTermError>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeTerm {
    pub kind: TypeTermKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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
    Float,
    String,
    CString,
    CPtr,
    Module,
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
        item: Box<TypeTerm>,
    },
    Handler {
        effect: Box<TypeTerm>,
        input: Box<TypeTerm>,
        output: Box<TypeTerm>,
    },
    Mut {
        inner: Box<TypeTerm>,
    },
    Record {
        fields: Box<[TypeField]>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeModuleRef {
    pub spec: Box<str>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeDim {
    Unknown,
    Name(Box<str>),
    Int(u32),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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
        match &self.kind {
            TypeTermKind::Error => f.write_str("<error>"),
            TypeTermKind::Unknown => f.write_str("Unknown"),
            TypeTermKind::Type => f.write_str("Type"),
            TypeTermKind::Syntax => f.write_str("Syntax"),
            TypeTermKind::Any => f.write_str("Any"),
            TypeTermKind::Empty => f.write_str("Empty"),
            TypeTermKind::Unit => f.write_str("Unit"),
            TypeTermKind::Bool => f.write_str("Bool"),
            TypeTermKind::Nat => f.write_str("Nat"),
            TypeTermKind::Int => f.write_str("Int"),
            TypeTermKind::Float => f.write_str("Float"),
            TypeTermKind::String => f.write_str("String"),
            TypeTermKind::CString => f.write_str("CString"),
            TypeTermKind::CPtr => f.write_str("CPtr"),
            TypeTermKind::Module => f.write_str("Module"),
            TypeTermKind::NatLit(value) => write!(f, "{value}"),
            TypeTermKind::Named { name, args, .. } => {
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
            TypeTermKind::Pi {
                binder,
                binder_ty,
                body,
                is_effectful,
            } => write!(
                f,
                "forall ({binder} : {binder_ty}) {} {body}",
                if *is_effectful { "~>" } else { "->" }
            ),
            TypeTermKind::Arrow {
                params,
                ret,
                is_effectful,
            } => {
                let params = params.iter().map(ToString::to_string).collect::<Vec<_>>();
                let left = if params.len() == 1 {
                    params[0].clone()
                } else {
                    format!("({})", params.join(", "))
                };
                write!(
                    f,
                    "{left} {} {ret}",
                    if *is_effectful { "~>" } else { "->" }
                )
            }
            TypeTermKind::Sum { left, right } => write!(f, "{left} + {right}"),
            TypeTermKind::Tuple { items } => write!(
                f,
                "({})",
                items
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            TypeTermKind::Seq { item } => write!(f, "[]{item}"),
            TypeTermKind::Array { dims, item } => write!(
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
            ),
            TypeTermKind::Range { item } => write!(f, "Range[{item}]"),
            TypeTermKind::Handler {
                effect,
                input,
                output,
            } => write!(f, "using {effect} ({input} -> {output})"),
            TypeTermKind::Mut { inner } => write!(f, "mut {inner}"),
            TypeTermKind::Record { fields } => write!(
                f,
                "{{{}}}",
                fields
                    .iter()
                    .map(|field| format!("{} = {}", field.name, field.ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
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

    fn parse_prefix(&mut self) -> TypeTermResult<TypeTerm> {
        self.skip_ws();
        if self.consume("mut") {
            self.require_ws()?;
            let inner = self.parse_prefix()?;
            return Ok(TypeTerm::new(TypeTermKind::Mut {
                inner: Box::new(inner),
            }));
        }
        if self.consume("using") {
            self.require_ws()?;
            let effect = self.parse_sum()?;
            self.expect("(")?;
            let input = self.parse_sum()?;
            self.skip_ws();
            self.expect("->")?;
            let output = self.parse_sum()?;
            self.expect(")")?;
            return Ok(TypeTerm::new(TypeTermKind::Handler {
                effect: Box::new(effect),
                input: Box::new(input),
                output: Box::new(output),
            }));
        }
        if self.consume("forall") {
            self.require_ws()?;
            self.expect("(")?;
            let binder = self.parse_ident()?;
            self.skip_ws();
            self.expect(":")?;
            let binder_ty = self.parse_sum()?;
            self.expect(")")?;
            self.skip_ws();
            let is_effectful = if self.consume("~>") {
                true
            } else {
                self.expect("->")?;
                false
            };
            let body = self.parse_sum()?;
            return Ok(TypeTerm::new(TypeTermKind::Pi {
                binder: binder.into(),
                binder_ty: Box::new(binder_ty),
                body: Box::new(body),
                is_effectful,
            }));
        }
        self.parse_atom()
    }

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
    match name {
        "Error" => Some(TypeTermKind::Error),
        "Unknown" => Some(TypeTermKind::Unknown),
        "Type" => Some(TypeTermKind::Type),
        "Syntax" => Some(TypeTermKind::Syntax),
        "Any" => Some(TypeTermKind::Any),
        "Empty" => Some(TypeTermKind::Empty),
        "Unit" => Some(TypeTermKind::Unit),
        "Bool" => Some(TypeTermKind::Bool),
        "Nat" => Some(TypeTermKind::Nat),
        "Int" => Some(TypeTermKind::Int),
        "Float" => Some(TypeTermKind::Float),
        "String" => Some(TypeTermKind::String),
        "CString" => Some(TypeTermKind::CString),
        "CPtr" => Some(TypeTermKind::CPtr),
        "Module" => Some(TypeTermKind::Module),
        _ => None,
    }
}
