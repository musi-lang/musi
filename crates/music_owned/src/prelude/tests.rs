use super::*;
use crate::types::BuiltinType;
use music_ast::ExprId;
use music_ast::common::{MemberDecl, MemberName, OpFixity};
use music_ast::data::AstData;
use music_ast::expr::{ClassDefData, DataBody, ExprKind};
use music_ast::pat::PatKind;
use music_lex::Lexer;
use music_parse::parse;
use music_shared::Interner;

fn parse_prelude() -> (AstData, Interner) {
    let (tokens, lex_errors) = Lexer::new(PRELUDE_SOURCE).lex();
    assert!(lex_errors.is_empty(), "prelude lex errors: {lex_errors:?}");
    let mut interner = Interner::new();
    let (ast, parse_errors) = parse(&tokens, PRELUDE_SOURCE, &mut interner);
    assert!(
        parse_errors.is_empty(),
        "prelude parse errors: {parse_errors:?}"
    );
    (ast, interner)
}

fn top_level_name(ast: &AstData, interner: &Interner, expr_id: ExprId) -> Option<String> {
    let expr = ast.exprs.get(expr_id);
    let ExprKind::Let(binding) = &expr.kind else {
        return None;
    };
    let pat = ast.pats.get(binding.pat);
    let PatKind::Bind(ident) = &pat.kind else {
        return None;
    };
    Some(interner.resolve(ident.name).to_owned())
}

fn top_level_class<'a>(
    ast: &'a AstData,
    interner: &Interner,
    name: &str,
) -> Option<&'a ClassDefData> {
    let expr_id = ast
        .root
        .iter()
        .copied()
        .find(|&expr_id| top_level_name(ast, interner, expr_id).as_deref() == Some(name))?;
    let expr = ast.exprs.get(expr_id);
    let ExprKind::Let(binding) = &expr.kind else {
        return None;
    };
    let value_id = binding.value?;
    let value = ast.exprs.get(value_id);
    let ExprKind::ClassDef(data) = &value.kind else {
        return None;
    };
    Some(data.as_ref())
}

fn member_source_name(interner: &Interner, member: &MemberDecl) -> Option<String> {
    let MemberDecl::Fn(decl) = member else {
        return None;
    };
    match &decl.name {
        MemberName::Ident(ident) => Some(interner.resolve(ident.name).to_owned()),
        MemberName::Op(ident, OpFixity::Infix | OpFixity::Prefix) => {
            Some(format!("({})", interner.resolve(ident.name)))
        }
    }
}

#[test]
fn prelude_classes_count() {
    assert_eq!(PRELUDE_CLASSES.len(), 5);
}

#[test]
fn eq_class_has_two_methods() {
    let eq = PRELUDE_CLASSES
        .iter()
        .find(|c| c.name == "Eq")
        .expect("Eq class missing");
    assert_eq!(eq.methods.len(), 2);
}

#[test]
fn ord_class_has_four_methods() {
    let ord = PRELUDE_CLASSES
        .iter()
        .find(|c| c.name == "Ord")
        .expect("Ord class missing");
    assert_eq!(ord.methods.len(), 4);
}

#[test]
fn num_class_has_six_methods() {
    let num = PRELUDE_CLASSES
        .iter()
        .find(|c| c.name == "Num")
        .expect("Num class missing");
    assert_eq!(num.methods.len(), 6);
}

#[test]
fn bits_class_has_two_methods() {
    let bits = PRELUDE_CLASSES
        .iter()
        .find(|c| c.name == "Bits")
        .expect("Bits class missing");
    assert_eq!(bits.methods.len(), 2);
}

#[test]
fn show_class_has_no_methods() {
    let show = PRELUDE_CLASSES
        .iter()
        .find(|c| c.name == "Show")
        .expect("Show class missing");
    assert_eq!(show.methods.len(), 1);
    assert_eq!(show.methods[0].name, "show");
}

#[test]
fn all_methods_have_nonempty_fields() {
    for class in PRELUDE_CLASSES {
        assert!(!class.name.is_empty(), "class name is empty");
        for method in class.methods {
            assert!(
                !method.name.is_empty(),
                "method name is empty in {}",
                class.name
            );
            assert!(
                !method.source_name.is_empty(),
                "source_name is empty for {}.{}",
                class.name,
                method.name
            );
            assert!(
                !method.op_name.is_empty(),
                "op_name is empty for {}.{}",
                class.name,
                method.name
            );
        }
    }
}

#[test]
fn builtin_variants_are_defined() {
    assert_eq!(BUILTIN_VARIANTS.len(), 2);
    assert_eq!(BUILTIN_VARIANTS[0].name, "False");
    assert_eq!(BUILTIN_VARIANTS[1].name, "True");
}

#[test]
fn prelude_source_exports_builtin_types_and_classes() {
    let (ast, interner) = parse_prelude();
    let top_level: Vec<_> = ast
        .root
        .iter()
        .filter_map(|&expr_id| top_level_name(&ast, &interner, expr_id))
        .collect();

    for builtin in BuiltinType::ALL {
        assert!(
            top_level.iter().any(|name| name == builtin.name()),
            "expected builtin {} in prelude source",
            builtin.name()
        );
    }

    for class in PRELUDE_CLASSES {
        assert!(
            top_level.iter().any(|name| name == class.name),
            "expected class {} in prelude source",
            class.name
        );
    }
}

#[test]
fn prelude_source_bool_variants_match_builtin_variants() {
    let (ast, interner) = parse_prelude();
    let bool_expr_id = ast
        .root
        .iter()
        .copied()
        .find(|&expr_id| top_level_name(&ast, &interner, expr_id).as_deref() == Some("Bool"))
        .expect("Bool declaration missing");
    let bool_expr = ast.exprs.get(bool_expr_id);
    let ExprKind::Let(binding) = &bool_expr.kind else {
        panic!("Bool must be a let binding");
    };
    let value_id = binding.value.expect("Bool value");
    let value = ast.exprs.get(value_id);
    let ExprKind::DataDef(body) = &value.kind else {
        panic!("Bool must be a data definition");
    };
    let DataBody::Sum(variants) = body.as_ref() else {
        panic!("Bool must be a sum type");
    };

    let parsed_variant_names: Vec<_> = variants
        .iter()
        .map(|variant| interner.resolve(variant.name.name).to_owned())
        .collect();
    let spec_variant_names: Vec<_> = BUILTIN_VARIANTS
        .iter()
        .map(|variant| variant.name)
        .collect();

    assert_eq!(parsed_variant_names.len(), BUILTIN_VARIANTS.len());
    for name in spec_variant_names {
        assert!(
            parsed_variant_names.iter().any(|variant| variant == name),
            "expected builtin Bool variant {name} in prelude source"
        );
    }
}

#[test]
fn builtin_method_opcode_specs_cover_prelude_methods() {
    let (ast, interner) = parse_prelude();
    for class in PRELUDE_CLASSES {
        let class_def = top_level_class(&ast, &interner, class.name)
            .unwrap_or_else(|| panic!("expected class {} in prelude source", class.name));
        let source_members: Vec<_> = class_def
            .members
            .iter()
            .filter_map(|member| member_source_name(&interner, member))
            .collect();
        for method in class.methods {
            if method.opcode.is_some() {
                assert!(
                    source_members.iter().any(|name| name == method.source_name),
                    "expected builtin method {} in prelude source for class {}",
                    method.source_name,
                    class.name
                );
            }
        }
    }
}
