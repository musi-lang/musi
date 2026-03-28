use std::collections::HashMap;

use music_ast::ExprId;
use music_ast::common::{Attr, AttrArg};
use music_ast::expr::ExprKind;
use music_db::Db;
use music_shared::Literal;

pub struct AttrSpec {
    pub path: &'static [&'static str],
    pub params: &'static [&'static str],
    pub required: &'static [&'static str],
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttrBindError {
    UnknownArgument { name: String },
    DuplicateArgument { name: String },
    TooManyArguments { expected: usize, found: usize },
    MissingArgument { name: &'static str },
}

pub struct BoundAttr {
    values: HashMap<&'static str, ExprId>,
}

impl BoundAttr {
    #[must_use]
    pub fn get(&self, name: &str) -> Option<ExprId> {
        self.values.get(name).copied()
    }
}

#[must_use]
pub fn attr_path_matches(db: &Db, attr: &Attr, path: &[&str]) -> bool {
    attr.path.len() == path.len()
        && attr
            .path
            .iter()
            .zip(path)
            .all(|(segment, expected)| db.interner.resolve(segment.name) == *expected)
}

pub fn bind_attr(db: &Db, attr: &Attr, spec: &AttrSpec) -> Result<BoundAttr, AttrBindError> {
    let mut values = HashMap::new();
    let mut positional_index = 0usize;

    for arg in &attr.args {
        match arg {
            AttrArg::Positional(expr_id) => {
                let Some(&name) = spec.params.get(positional_index) else {
                    return Err(AttrBindError::TooManyArguments {
                        expected: spec.params.len(),
                        found: positional_index + 1,
                    });
                };
                if values.insert(name, *expr_id).is_some() {
                    return Err(AttrBindError::DuplicateArgument {
                        name: name.to_owned(),
                    });
                }
                positional_index += 1;
            }
            AttrArg::Named { name, value } => {
                let resolved = db.interner.resolve(name.name);
                let Some(&param) = spec.params.iter().find(|param| **param == resolved) else {
                    return Err(AttrBindError::UnknownArgument {
                        name: resolved.to_owned(),
                    });
                };
                if values.insert(param, *value).is_some() {
                    return Err(AttrBindError::DuplicateArgument {
                        name: resolved.to_owned(),
                    });
                }
            }
        }
    }

    for &required in spec.required {
        if !values.contains_key(required) {
            return Err(AttrBindError::MissingArgument { name: required });
        }
    }

    Ok(BoundAttr { values })
}

#[must_use]
pub fn attr_expr_string(db: &Db, expr_id: ExprId) -> Option<String> {
    match &db.ast.exprs.get(expr_id).kind {
        ExprKind::Lit(Literal::Str(value)) => Some(value.clone()),
        _ => None,
    }
}
