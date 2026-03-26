use music_shared::{Ident, Interner, Literal, Span, Symbol};

use super::*;
use crate::common::{Param, Signature};

fn test_ident() -> (Interner, Ident) {
    let mut interner = Interner::new();
    let sym = interner.intern("x");
    (interner, Ident::new(sym, Span::DUMMY))
}

fn test_symbol() -> (Interner, Symbol) {
    let mut interner = Interner::new();
    let sym = interner.intern("std/io");
    (interner, sym)
}

fn dummy_expr_id() -> ExprId {
    ExprId::from_raw(0)
}

fn dummy_pat_id() -> PatId {
    PatId::from_raw(0)
}

fn dummy_ty_id() -> TyId {
    TyId::from_raw(0)
}

#[test]
fn lit_variant() {
    let e = ExprKind::Lit(Literal::Int(42));
    assert!(matches!(e, ExprKind::Lit(Literal::Int(42))));
}

#[test]
fn variant_lit() {
    let (_i, ident) = test_ident();
    let e = ExprKind::VariantLit(ident, vec![dummy_expr_id()]);
    assert!(matches!(e, ExprKind::VariantLit(_, ref args) if args.len() == 1));
}

#[test]
fn tuple_lit() {
    let e = ExprKind::TupleLit(vec![]);
    assert!(matches!(e, ExprKind::TupleLit(ref elems) if elems.is_empty()));
}

#[test]
fn array_lit() {
    let e = ExprKind::ArrayLit(vec![dummy_expr_id()]);
    assert!(matches!(e, ExprKind::ArrayLit(ref elems) if elems.len() == 1));
}

#[test]
fn matrix_lit() {
    let e = ExprKind::MatrixLit(vec![vec![dummy_expr_id()], vec![dummy_expr_id()]]);
    assert!(matches!(e, ExprKind::MatrixLit(ref rows) if rows.len() == 2));
}

#[test]
fn record_lit() {
    let (_i, ident) = test_ident();
    let field = RecordField::Named {
        name: ident,
        value: Some(dummy_expr_id()),
    };
    let e = ExprKind::RecordLit(vec![field]);
    assert!(matches!(e, ExprKind::RecordLit(ref fields) if fields.len() == 1));
}

#[test]
fn record_field_spread() {
    let field = RecordField::Spread(dummy_expr_id());
    assert!(matches!(field, RecordField::Spread(_)));
}

#[test]
fn fstr_lit() {
    let parts = vec![
        FStrPart::Lit(String::from("hello ")),
        FStrPart::Expr(dummy_expr_id()),
    ];
    let e = ExprKind::FStrLit(parts);
    assert!(matches!(e, ExprKind::FStrLit(ref p) if p.len() == 2));
}

#[test]
fn var() {
    let (_i, ident) = test_ident();
    let e = ExprKind::Var(ident);
    assert!(matches!(e, ExprKind::Var(_)));
}

#[test]
fn app() {
    let e = ExprKind::App(dummy_expr_id(), vec![dummy_expr_id()]);
    assert!(matches!(e, ExprKind::App(_, ref args) if args.len() == 1));
}

#[test]
fn bin_op_expr() {
    let e = ExprKind::BinOp(BinOp::Add, dummy_expr_id(), dummy_expr_id());
    assert!(matches!(e, ExprKind::BinOp(BinOp::Add, _, _)));
}

#[test]
fn unary_op_expr() {
    let e = ExprKind::UnaryOp(UnaryOp::Neg, dummy_expr_id());
    assert!(matches!(e, ExprKind::UnaryOp(UnaryOp::Neg, _)));
}

#[test]
fn access_direct() {
    let (_i, ident) = test_ident();
    let e = ExprKind::Access {
        expr: dummy_expr_id(),
        field: FieldTarget::Name(ident),
        mode: AccessMode::Direct,
    };
    assert!(matches!(
        e,
        ExprKind::Access {
            mode: AccessMode::Direct,
            ..
        }
    ));
}

#[test]
fn index_point() {
    let e = ExprKind::Index {
        expr: dummy_expr_id(),
        indices: vec![dummy_expr_id()],
        kind: IndexKind::Point,
    };
    assert!(matches!(
        e,
        ExprKind::Index {
            kind: IndexKind::Point,
            ..
        }
    ));
}

#[test]
fn type_op_test() {
    let e = ExprKind::TypeOp {
        expr: dummy_expr_id(),
        ty: dummy_ty_id(),
        kind: TypeOpKind::Test(None),
    };
    assert!(matches!(e, ExprKind::TypeOp { .. }));
}

#[test]
fn postfix_force() {
    let e = ExprKind::Postfix {
        expr: dummy_expr_id(),
        op: PostfixOp::Force,
    };
    assert!(matches!(
        e,
        ExprKind::Postfix {
            op: PostfixOp::Force,
            ..
        }
    ));
}

#[test]
fn seq() {
    let e = ExprKind::Seq(vec![dummy_expr_id(), dummy_expr_id()]);
    assert!(matches!(e, ExprKind::Seq(ref exprs) if exprs.len() == 2));
}

#[test]
fn comprehension() {
    let e = ExprKind::Comprehension(Box::new(ComprehensionData {
        expr: dummy_expr_id(),
        clauses: vec![CompClause::Filter(dummy_expr_id())],
    }));
    assert!(matches!(e, ExprKind::Comprehension(_)));
}

#[test]
fn comp_clause_generator() {
    let c = CompClause::Generator {
        pat: dummy_pat_id(),
        iter: dummy_expr_id(),
    };
    assert!(matches!(c, CompClause::Generator { .. }));
}

#[test]
fn let_binding_with_sig() {
    let (_i, ident) = test_ident();
    let sig = Signature {
        params: vec![Param {
            mutable: false,
            name: ident,
            ty: Some(dummy_ty_id()),
            default: None,
        }],
        ty_params: vec![],
        constraints: vec![],
        effects: vec![],
        ret_ty: Some(dummy_ty_id()),
    };
    let lb = LetBinding {
        modifiers: ModifierSet::default(),
        attrs: vec![],
        pat: dummy_pat_id(),
        sig: Some(Box::new(sig)),
        value: Some(dummy_expr_id()),
    };
    let e = ExprKind::Let(Box::new(lb));
    assert!(matches!(e, ExprKind::Let(_)));
}

#[test]
fn let_binding_minimal() {
    let lb = LetBinding {
        modifiers: ModifierSet::default(),
        attrs: vec![],
        pat: dummy_pat_id(),
        sig: None,
        value: Some(dummy_expr_id()),
    };
    assert!(lb.sig.is_none());
    assert!(lb.value.is_some());
}

#[test]
fn let_binding_with_modifiers() {
    let (_int, sym) = test_symbol();
    let lb = LetBinding {
        modifiers: ModifierSet {
            exported: true,
            opaque: false,
            mutable: false,
            foreign_abi: Some(sym),
            foreign_alias: Some(sym),
        },
        attrs: vec![],
        pat: dummy_pat_id(),
        sig: None,
        value: None,
    };
    assert!(lb.modifiers.exported);
    assert!(lb.modifiers.foreign_abi.is_some());
    assert!(lb.value.is_none());
}

#[test]
fn assign() {
    let e = ExprKind::Assign(dummy_expr_id(), dummy_expr_id());
    assert!(matches!(e, ExprKind::Assign(_, _)));
}

#[test]
fn lambda() {
    let e = ExprKind::Lambda {
        params: vec![],
        ret_ty: None,
        body: dummy_expr_id(),
    };
    assert!(matches!(e, ExprKind::Lambda { .. }));
}

#[test]
fn case_expr() {
    let arm = CaseArm {
        attrs: vec![],
        pat: dummy_pat_id(),
        guard: None,
        body: dummy_expr_id(),
    };
    let e = ExprKind::Case(Box::new(CaseData {
        scrutinee: dummy_expr_id(),
        arms: vec![arm],
    }));
    assert!(matches!(e, ExprKind::Case(ref data) if data.arms.len() == 1));
}

#[test]
fn return_some() {
    let e = ExprKind::Return(Some(dummy_expr_id()));
    assert!(matches!(e, ExprKind::Return(Some(_))));
}

#[test]
fn return_none() {
    let e = ExprKind::Return(None);
    assert!(matches!(e, ExprKind::Return(None)));
}

#[test]
fn resume() {
    let e = ExprKind::Resume(Some(dummy_expr_id()));
    assert!(matches!(e, ExprKind::Resume(Some(_))));
}

#[test]
fn import_qualified() {
    let (_i, ident) = test_ident();
    let (_si, sym) = test_symbol();
    let e = ExprKind::Import {
        path: sym,
        kind: ImportKind::Qualified(ident),
    };
    assert!(matches!(e, ExprKind::Import { .. }));
}

#[test]
fn import_wildcard() {
    let (_i, sym) = test_symbol();
    let e = ExprKind::Import {
        path: sym,
        kind: ImportKind::Wildcard,
    };
    assert!(matches!(
        e,
        ExprKind::Import {
            kind: ImportKind::Wildcard,
            ..
        }
    ));
}

#[test]
fn import_selective() {
    let (_i, ident) = test_ident();
    let kind = ImportKind::Selective(ident, vec![ident]);
    assert!(matches!(kind, ImportKind::Selective(_, _)));
}

#[test]
fn need() {
    let e = ExprKind::Need(dummy_expr_id());
    assert!(matches!(e, ExprKind::Need(_)));
}

#[test]
fn piecewise() {
    let arm = PiecewiseArm {
        value: dummy_expr_id(),
        guard: PwGuard::Wildcard,
    };
    let e = ExprKind::Piecewise(vec![arm]);
    assert!(matches!(e, ExprKind::Piecewise(ref arms) if arms.len() == 1));
}

#[test]
fn pw_guard_expr() {
    let g = PwGuard::Expr(dummy_expr_id());
    assert!(matches!(g, PwGuard::Expr(_)));
}

#[test]
fn branch() {
    let e = ExprKind::Branch {
        cond: dummy_expr_id(),
        then_br: dummy_expr_id(),
        else_br: dummy_expr_id(),
    };
    assert!(matches!(e, ExprKind::Branch { .. }));
}

#[test]
fn quote_expr() {
    let e = ExprKind::Quote(QuoteKind::Expr(dummy_expr_id()));
    assert!(matches!(e, ExprKind::Quote(QuoteKind::Expr(_))));
}

#[test]
fn quote_block() {
    let e = ExprKind::Quote(QuoteKind::Block(vec![]));
    assert!(matches!(e, ExprKind::Quote(QuoteKind::Block(_))));
}

#[test]
fn splice_ident() {
    let (_i, ident) = test_ident();
    let e = ExprKind::Splice(SpliceKind::Ident(ident));
    assert!(matches!(e, ExprKind::Splice(SpliceKind::Ident(_))));
}

#[test]
fn splice_expr() {
    let e = ExprKind::Splice(SpliceKind::Expr(dummy_expr_id()));
    assert!(matches!(e, ExprKind::Splice(SpliceKind::Expr(_))));
}

#[test]
fn splice_array() {
    let e = ExprKind::Splice(SpliceKind::Array(vec![]));
    assert!(matches!(e, ExprKind::Splice(SpliceKind::Array(_))));
}

#[test]
fn foreign_import() {
    let (_i, sym) = test_symbol();
    let kind = ExprKind::ForeignImport(sym);
    assert!(matches!(kind, ExprKind::ForeignImport(_)));
}

#[test]
fn instance_def_via() {
    let (_i, ident) = test_ident();
    let e = ExprKind::InstanceDef(Box::new(InstanceDef {
        attrs: Vec::new(),
        exported: false,
        ty_params: vec![],
        constraints: vec![],
        ty: TyRef {
            name: ident,
            args: vec![],
        },
        body: InstanceBody::Via(TyRef {
            name: ident,
            args: vec![],
        }),
    }));
    assert!(matches!(e, ExprKind::InstanceDef(_)));
}

#[test]
fn instance_def_methods() {
    let (_i, ident) = test_ident();
    let inst = InstanceDef {
        attrs: Vec::new(),
        exported: true,
        ty_params: vec![ident],
        constraints: vec![],
        ty: TyRef {
            name: ident,
            args: vec![],
        },
        body: InstanceBody::Methods(vec![]),
    };
    assert!(inst.exported);
    let e = ExprKind::InstanceDef(Box::new(inst));
    assert!(matches!(e, ExprKind::InstanceDef(_)));
}

#[test]
fn class_def() {
    let e = ExprKind::ClassDef(Box::new(ClassDefData {
        constraints: vec![],
        members: vec![],
    }));
    assert!(matches!(e, ExprKind::ClassDef(_)));
}

#[test]
fn data_def_product() {
    let e = ExprKind::DataDef(Box::new(DataBody::Product(vec![])));
    assert!(matches!(e, ExprKind::DataDef(ref b) if matches!(b.as_ref(), DataBody::Product(_))));
}

#[test]
fn data_def_sum() {
    let e = ExprKind::DataDef(Box::new(DataBody::Sum(vec![])));
    assert!(matches!(e, ExprKind::DataDef(ref b) if matches!(b.as_ref(), DataBody::Sum(_))));
}

#[test]
fn effect_def() {
    let e = ExprKind::EffectDef(vec![]);
    assert!(matches!(e, ExprKind::EffectDef(_)));
}

#[test]
fn bin_op_is_copy() {
    let op = BinOp::Add;
    let copied = op;
    assert_eq!(op, copied);
}

#[test]
fn unary_op_is_copy() {
    let op = UnaryOp::Neg;
    let copied = op;
    assert_eq!(op, copied);
}

#[test]
fn access_mode_is_copy_eq() {
    let m = AccessMode::Optional;
    let copied = m;
    assert_eq!(m, copied);
}

#[test]
fn index_kind_is_copy_eq() {
    let k = IndexKind::Slice;
    let copied = k;
    assert_eq!(k, copied);
}

#[test]
fn postfix_op_is_copy_eq() {
    let op = PostfixOp::Propagate;
    let copied = op;
    assert_eq!(op, copied);
}

#[test]
fn pw_guard_is_copy_eq() {
    let g = PwGuard::Wildcard;
    let copied = g;
    assert_eq!(g, copied);
}

#[test]
fn all_bin_ops() {
    let ops = [
        BinOp::NilCoalesce,
        BinOp::PipeRight,
        BinOp::Or,
        BinOp::Xor,
        BinOp::And,
        BinOp::Eq,
        BinOp::NotEq,
        BinOp::Lt,
        BinOp::Gt,
        BinOp::LtEq,
        BinOp::GtEq,
        BinOp::Range,
        BinOp::RangeExcl,
        BinOp::Cons,
        BinOp::Add,
        BinOp::Sub,
        BinOp::Mul,
        BinOp::Div,
        BinOp::Rem,
    ];
    assert_eq!(ops.len(), 19);
}

#[test]
fn type_op_kind_cast() {
    let k = TypeOpKind::Cast;
    let copied = k;
    assert_eq!(k, copied);
}

#[test]
fn type_op_kind_test_with_binding() {
    let (_i, ident) = test_ident();
    let k = TypeOpKind::Test(Some(ident));
    assert!(matches!(k, TypeOpKind::Test(Some(_))));
}

#[test]
fn expr_kind_clone_eq() {
    let e = ExprKind::Lit(Literal::Int(1));
    let cloned = e.clone();
    assert_eq!(e, cloned);
}

#[test]
fn handle_expr() {
    use crate::common::{FnDecl, MemberName, TyRef};
    let (_i, ident) = test_ident();
    let e = ExprKind::Handle(Box::new(HandleData {
        effect: TyRef {
            name: ident,
            args: vec![],
        },
        handlers: vec![FnDecl {
            attrs: vec![],
            name: MemberName::Ident(ident),
            params: None,
            ret_ty: None,
            body: Some(dummy_expr_id()),
        }],
        body: dummy_expr_id(),
    }));
    assert!(matches!(e, ExprKind::Handle(_)));
}

#[test]
fn record_update() {
    let base = ExprId::from_raw(0);
    let kind = ExprKind::RecordUpdate {
        base,
        fields: vec![],
    };
    assert!(matches!(kind, ExprKind::RecordUpdate { .. }));
}

#[test]
fn expr_kind_size_is_reasonable() {
    use core::mem::size_of;
    let size = size_of::<ExprKind>();
    assert!(size <= 80, "ExprKind is {size} bytes, expected <= 80");
}
