use music_shared::{Ident, Interner, Literal, Span};

use super::*;

fn test_ident() -> (Interner, Ident) {
    let mut interner = Interner::new();
    let sym = interner.intern("x");
    (interner, Ident::new(sym, Span::DUMMY))
}

fn dummy_pat_id() -> PatId {
    PatId::from_raw(0)
}

#[test]
fn wildcard() {
    let p = PatKind::Wildcard;
    assert!(matches!(p, PatKind::Wildcard));
}

#[test]
fn lit_int() {
    let p = PatKind::Lit(Literal::Int(42));
    assert!(matches!(p, PatKind::Lit(Literal::Int(42))));
}

#[test]
fn lit_str() {
    let p = PatKind::Lit(Literal::Str(String::from("hello")));
    assert!(matches!(p, PatKind::Lit(Literal::Str(_))));
}

#[test]
fn bind() {
    let (_i, ident) = test_ident();
    let p = PatKind::Bind(ident);
    assert!(matches!(p, PatKind::Bind(_)));
}

#[test]
fn as_pat() {
    let (_i, ident) = test_ident();
    let p = PatKind::As {
        name: ident,
        pat: dummy_pat_id(),
    };
    assert!(matches!(p, PatKind::As { .. }));
}

#[test]
fn variant() {
    let (_i, ident) = test_ident();
    let p = PatKind::Variant {
        tag: ident,
        fields: vec![dummy_pat_id()],
    };
    assert!(matches!(p, PatKind::Variant { .. }));
}

#[test]
fn record() {
    let (_i, ident) = test_ident();
    let field = RecordPatField {
        mutable: false,
        name: ident,
        pat: None,
    };
    let p = PatKind::Record(vec![field]);
    assert!(matches!(p, PatKind::Record(ref fields) if fields.len() == 1));
}

#[test]
fn record_pat_field_with_pat() {
    let (_i, ident) = test_ident();
    let field = RecordPatField {
        mutable: true,
        name: ident,
        pat: Some(dummy_pat_id()),
    };
    assert!(field.mutable);
    assert!(field.pat.is_some());
}

#[test]
fn tuple() {
    let p = PatKind::Tuple(vec![dummy_pat_id(), dummy_pat_id()]);
    assert!(matches!(p, PatKind::Tuple(ref pats) if pats.len() == 2));
}

#[test]
fn array() {
    let p = PatKind::Array(vec![]);
    assert!(matches!(p, PatKind::Array(ref pats) if pats.is_empty()));
}

#[test]
fn or_pat() {
    let p = PatKind::Or(vec![dummy_pat_id(), dummy_pat_id()]);
    assert!(matches!(p, PatKind::Or(ref pats) if pats.len() == 2));
}

#[test]
fn pat_kind_clone_eq() {
    let p = PatKind::Wildcard;
    let cloned = p.clone();
    assert_eq!(p, cloned);
}

#[test]
fn record_pat_field_clone_eq() {
    let (_i, ident) = test_ident();
    let field = RecordPatField {
        mutable: false,
        name: ident,
        pat: None,
    };
    let cloned = field.clone();
    assert_eq!(field, cloned);
}
