use std::collections::HashMap;

use music_ast::common::{AttrArg, MemberDecl, MemberName};
use music_ast::data::AstData;
use music_ast::expr::ExprKind;
use music_ast::{AttrId, AttrList, ExprId};
use music_found::{Interner, Literal, Symbol};

/// Opcode mnemonics reachable via `@_intrinsic(opcode := "...")`.
///
/// Comparing against this table lets `extract_intrinsic_opcode` return
/// `&'static str` without heap-allocating.
const KNOWN_OPCODES: &[&str] = &[
    "shl",
    "shr",
    "ld.true",
    "ld.false",
    "i.add",
    "i.sub",
    "i.mul",
    "i.div",
    "i.mod",
    "f.add",
    "f.sub",
    "f.mul",
    "f.div",
    "and",
    "or",
    "xor",
    "not",
    "cmp.eq",
    "cmp.neq",
    "cmp.lt",
    "cmp.gt",
    "cmp.leq",
    "cmp.geq",
    "list.cons",
    "range.new",
    "option.unwrap_or",
];

/// Returns the `&'static str` for a known opcode mnemonic, or `None`.
#[must_use]
pub fn static_opcode(s: &str) -> Option<&'static str> {
    KNOWN_OPCODES.iter().find(|&&k| k == s).copied()
}

/// Extracts the `opcode` field from `@_intrinsic(...)` if present.
///
/// Supported forms:
/// - `@_intrinsic(opcode := "shl")` — named
/// - `@_intrinsic("name", "shl")` — positional, opcode at index 1
/// - `@_intrinsic("shl")` — single positional matched against `KNOWN_OPCODES`
#[must_use]
pub fn extract_intrinsic_opcode(
    ast: &AstData,
    interner: &Interner,
    attrs: &AttrList,
) -> Option<&'static str> {
    for &attr_id in attrs {
        if let Some(opcode) = check_attr(ast, interner, attr_id) {
            return Some(opcode);
        }
    }
    None
}

fn check_attr(ast: &AstData, interner: &Interner, attr_id: AttrId) -> Option<&'static str> {
    let attr = ast.attrs.get(attr_id);
    if interner.resolve(attr.kind.name.name) != "_intrinsic" {
        return None;
    }

    // Named arg: opcode := "..."
    for arg in &attr.kind.args {
        if let AttrArg::Named { name: kname, value } = arg {
            if interner.resolve(kname.name) == "opcode" {
                return extract_str_opcode(ast, *value);
            }
        }
    }

    // Positional args
    let positional: Vec<ExprId> = attr
        .kind
        .args
        .iter()
        .filter_map(|a| {
            if let AttrArg::Positional(e) = a {
                Some(*e)
            } else {
                None
            }
        })
        .collect();

    // @_intrinsic("name", "opcode") — opcode at index 1
    if positional.len() >= 2 {
        return extract_str_opcode(ast, positional[1]);
    }

    // @_intrinsic("opcode") — single positional treated as opcode if known
    if let [single] = positional.as_slice() {
        return extract_str_opcode(ast, *single);
    }

    None
}

fn extract_str_opcode(ast: &AstData, expr_id: ExprId) -> Option<&'static str> {
    let spanned = ast.exprs.get(expr_id);
    if let ExprKind::Lit(Literal::Str(ref s)) = spanned.kind {
        return static_opcode(s.as_str());
    }
    None
}

/// Maps collected from scanning `@_intrinsic` attributes in the AST.
pub struct IntrinsicMaps {
    /// Class/effect method symbols → opcode mnemonic.
    pub methods: HashMap<Symbol, &'static str>,
    /// Choice variant name symbols → opcode mnemonic.
    pub variants: HashMap<Symbol, &'static str>,
}

/// Scans the AST root for all `ClassDef`/`EffectDef` members and `ChoiceDef`
/// variants bearing `@_intrinsic(opcode := "...")`.
#[must_use]
pub fn collect_intrinsic_methods(ast: &AstData, interner: &Interner) -> IntrinsicMaps {
    let mut maps = IntrinsicMaps {
        methods: HashMap::new(),
        variants: HashMap::new(),
    };

    for &expr_id in &ast.root {
        let spanned = ast.exprs.get(expr_id);
        visit_expr(ast, interner, &spanned.kind, &mut maps);
    }

    maps
}

fn visit_expr(ast: &AstData, interner: &Interner, kind: &ExprKind, maps: &mut IntrinsicMaps) {
    match kind {
        ExprKind::ClassDef { members, .. } | ExprKind::EffectDef(members) => {
            collect_members(ast, interner, members, &mut maps.methods);
        }
        ExprKind::ChoiceDef(variants) => {
            for v in variants {
                if let Some(opcode) = extract_intrinsic_opcode(ast, interner, &v.attrs) {
                    let _ = maps.variants.insert(v.name.name, opcode);
                }
            }
        }
        ExprKind::Let(binding) => {
            if let Some(value_id) = binding.value {
                let inner = ast.exprs.get(value_id);
                visit_expr(ast, interner, &inner.kind, maps);
            }
        }
        _ => {}
    }
}

fn collect_members(
    ast: &AstData,
    interner: &Interner,
    members: &[MemberDecl],
    map: &mut HashMap<Symbol, &'static str>,
) {
    for member in members {
        if let MemberDecl::Fn(decl) = member {
            if let Some(opcode) = extract_intrinsic_opcode(ast, interner, &decl.attrs) {
                let name_sym = match &decl.name {
                    MemberName::Ident(i) | MemberName::Op(i) => i.name,
                };
                let _ = map.insert(name_sym, opcode);
            }
        }
    }
}
