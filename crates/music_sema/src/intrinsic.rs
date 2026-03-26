use std::collections::HashMap;

use music_ast::common::{AttrArg, MemberDecl, MemberName};
use music_ast::data::AstData;
use music_ast::expr::{DataBody, ExprKind};
use music_ast::{AttrId, AttrList, ExprId};
use music_found::{Interner, Literal, Symbol};
use music_il::opcode::Opcode;

/// Extracts the `opcode` field from `@builtin(opcode := 0x14)` if present.
///
/// Returns the `Opcode` variant corresponding to the byte value, or `None`
/// if no `@builtin` attribute with an `opcode` argument is found.
#[must_use]
pub fn extract_builtin_opcode(
    ast: &AstData,
    interner: &Interner,
    attrs: &AttrList,
) -> Option<Opcode> {
    for &attr_id in attrs {
        if let Some(opcode) = check_attr(ast, interner, attr_id) {
            return Some(opcode);
        }
    }
    None
}

fn check_attr(ast: &AstData, interner: &Interner, attr_id: AttrId) -> Option<Opcode> {
    let attr = ast.attrs.get(attr_id);
    if interner.resolve(attr.kind.name.name) != "builtin" {
        return None;
    }

    // Named arg: opcode := 0x14
    for arg in &attr.kind.args {
        if let AttrArg::Named { name: kname, value } = arg {
            if interner.resolve(kname.name) == "opcode" {
                return extract_int_opcode(ast, *value);
            }
        }
    }

    // Positional: @builtin(0x14)
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

    if let [single] = positional.as_slice() {
        return extract_int_opcode(ast, *single);
    }

    None
}

fn extract_int_opcode(ast: &AstData, expr_id: ExprId) -> Option<Opcode> {
    let spanned = ast.exprs.get(expr_id);
    if let ExprKind::Lit(Literal::Int(n)) = &spanned.kind {
        let byte = u8::try_from(*n).ok()?;
        return Opcode::from_byte(byte);
    }
    None
}

/// Maps collected from scanning `@builtin` attributes in the AST.
pub struct BuiltinMaps {
    /// Class/effect method symbols → opcode.
    pub methods: HashMap<Symbol, Opcode>,
    /// Sum variant name symbols → opcode.
    pub variants: HashMap<Symbol, Opcode>,
}

/// Scans the AST root for all `ClassDef`/`EffectDef` members and `DataDef(Sum)`
/// variants bearing `@builtin(opcode := ...)`.
#[must_use]
pub fn collect_builtin_methods(ast: &AstData, interner: &Interner) -> BuiltinMaps {
    let mut maps = BuiltinMaps {
        methods: HashMap::new(),
        variants: HashMap::new(),
    };

    for &expr_id in &ast.root {
        let spanned = ast.exprs.get(expr_id);
        visit_expr(ast, interner, &spanned.kind, &mut maps);
    }

    maps
}

fn visit_expr(ast: &AstData, interner: &Interner, kind: &ExprKind, maps: &mut BuiltinMaps) {
    match kind {
        ExprKind::ClassDef { members, .. } | ExprKind::EffectDef(members) => {
            collect_members(ast, interner, members, &mut maps.methods);
        }
        ExprKind::DataDef(DataBody::Sum(variants)) => {
            for v in variants {
                if let Some(opcode) = extract_builtin_opcode(ast, interner, &v.attrs) {
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
    map: &mut HashMap<Symbol, Opcode>,
) {
    for member in members {
        if let MemberDecl::Fn(decl) = member {
            if let Some(opcode) = extract_builtin_opcode(ast, interner, &decl.attrs) {
                let name_sym = match &decl.name {
                    MemberName::Ident(i) | MemberName::Op(i, _) => i.name,
                };
                let _ = map.insert(name_sym, opcode);
            }
        }
    }
}
