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
fn modifier_set_default() {
    let m = ModifierSet::default();
    assert!(!m.exported);
    assert!(!m.opaque);
    assert!(!m.mutable);
    assert!(m.foreign_abi.is_none());
    assert!(m.foreign_alias.is_none());
}

#[test]
fn modifier_set_is_copy() {
    let m = ModifierSet {
        exported: true,
        opaque: false,
        mutable: true,
        foreign_abi: None,
        foreign_alias: None,
    };
    let copied = m;
    assert_eq!(m, copied);
}

#[test]
fn signature_construction() {
    let (_i, ident) = test_ident();
    let sig = Signature {
        params: vec![Param {
            mutable: false,
            name: ident,
            ty: Some(dummy_ty_id()),
            default: None,
        }],
        ty_params: vec![ident],
        constraints: vec![],
        effects: vec![],
        ret_ty: Some(dummy_ty_id()),
    };
    assert_eq!(sig.params.len(), 1);
    assert_eq!(sig.ty_params.len(), 1);
    assert!(sig.ret_ty.is_some());
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
fn constraint_implements() {
    let (_i, ident) = test_ident();
    let c = Constraint::Implements {
        ty: ident,
        class: TyRef {
            name: ident,
            args: vec![],
        },
    };
    assert!(matches!(c, Constraint::Implements { .. }));
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
fn effect_item_construction() {
    let (_i, ident) = test_ident();
    let ei = EffectItem {
        name: ident,
        arg: Some(dummy_ty_id()),
    };
    assert!(ei.arg.is_some());
}

#[test]
fn member_decl_fn() {
    let (_i, ident) = test_ident();
    let md = MemberDecl::Fn(FnDecl {
        attrs: vec![],
        name: MemberName::Ident(ident),
        params: None,
        ret_ty: None,
        body: None,
    });
    assert!(matches!(md, MemberDecl::Fn(_)));
}

#[test]
fn member_decl_law() {
    let (_i, ident) = test_ident();
    let md = MemberDecl::Law(LawDecl {
        name: ident,
        params: None,
        body: dummy_expr_id(),
    });
    assert!(matches!(md, MemberDecl::Law(_)));
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
        attrs: vec![],
        name: ident,
        payload: None,
    };
    let cloned = v.clone();
    assert_eq!(v, cloned);
}
