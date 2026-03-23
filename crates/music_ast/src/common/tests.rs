use music_found::{Ident, Interner, Span};

use super::*;

fn test_ident() -> (Interner, Ident) {
    let mut interner = Interner::new();
    let sym = interner.intern("x");
    (interner, Ident::new(sym, Span::DUMMY))
}

fn dummy_expr_id() -> ExprId {
    ExprId::from_raw(0)
}

fn dummy_ty_id() -> TyId {
    TyId::from_raw(0)
}

#[test]
fn param_construction() {
    let (_i, ident) = test_ident();
    let param = Param {
        mutable: true,
        name: ident,
        ty: Some(dummy_ty_id()),
        default: None,
    };
    assert!(param.mutable);
    assert!(param.ty.is_some());
    assert!(param.default.is_none());
}

#[test]
fn param_clone_eq() {
    let (_i, ident) = test_ident();
    let param = Param {
        mutable: false,
        name: ident,
        ty: None,
        default: Some(dummy_expr_id()),
    };
    let cloned = param.clone();
    assert_eq!(param, cloned);
}

#[test]
fn attr_construction() {
    let (_i, ident) = test_ident();
    let attr = Attr {
        name: ident,
        args: vec![AttrArg::Positional(dummy_expr_id())],
    };
    assert_eq!(attr.args.len(), 1);
}

#[test]
fn attr_named_arg() {
    let (_i, ident) = test_ident();
    let arg = AttrArg::Named {
        name: ident,
        value: dummy_expr_id(),
    };
    assert!(matches!(arg, AttrArg::Named { .. }));
}

#[test]
fn where_clause_construction() {
    let (_i, ident) = test_ident();
    let wc = WhereClause {
        constraints: vec![Constraint::Implements {
            ty: ident,
            class: TyRef {
                name: ident,
                args: vec![],
            },
        }],
    };
    assert_eq!(wc.constraints.len(), 1);
}

#[test]
fn constraint_subtype() {
    let (_i, ident) = test_ident();
    let c = Constraint::Subtype {
        ty: ident,
        bound: TyRef {
            name: ident,
            args: vec![dummy_ty_id()],
        },
    };
    assert!(matches!(c, Constraint::Subtype { .. }));
}

#[test]
fn ty_ref_clone_eq() {
    let (_i, ident) = test_ident();
    let tr = TyRef {
        name: ident,
        args: vec![dummy_ty_id()],
    };
    let cloned = tr.clone();
    assert_eq!(tr, cloned);
}

#[test]
fn effect_set_construction() {
    let (_i, ident) = test_ident();
    let es = EffectSet {
        effects: vec![EffectItem {
            name: ident,
            arg: Some(dummy_ty_id()),
        }],
    };
    assert_eq!(es.effects.len(), 1);
}

#[test]
fn member_kind_fn() {
    let (_i, ident) = test_ident();
    let mk = MemberKind::Fn(FnDecl {
        name: MemberName::Ident(ident),
        params: None,
        ret_ty: None,
        body: None,
    });
    assert!(matches!(mk, MemberKind::Fn(_)));
}

#[test]
fn member_kind_law() {
    let (_i, ident) = test_ident();
    let mk = MemberKind::Law(LawDecl {
        name: ident,
        params: None,
        body: dummy_expr_id(),
    });
    assert!(matches!(mk, MemberKind::Law(_)));
}

#[test]
fn member_name_op() {
    let (_i, ident) = test_ident();
    let mn = MemberName::Op(ident);
    let cloned = mn.clone();
    assert_eq!(mn, cloned);
}

#[test]
fn record_def_field() {
    let (_i, ident) = test_ident();
    let f = RecordDefField {
        name: ident,
        ty: dummy_ty_id(),
        default: Some(dummy_expr_id()),
    };
    assert!(f.default.is_some());
}

#[test]
fn variant_def() {
    let (_i, ident) = test_ident();
    let v = VariantDef {
        name: ident,
        payload: None,
    };
    let cloned = v.clone();
    assert_eq!(v, cloned);
}

#[test]
fn foreign_binding_construction() {
    let (_i, ident) = test_ident();
    let fb = ForeignBinding {
        attrs: vec![],
        name: ident,
        ty_params: None,
        alias: Some(String::from("c_func")),
        where_clause: None,
        ty: Some(dummy_ty_id()),
    };
    assert_eq!(fb.alias.as_deref(), Some("c_func"));
}
