use music_base::SourceId;
use music_module::ModuleKey;
use music_names::Interner;
use music_resolve::{ResolveOptions, resolve_module};
use music_sema::{SemaOptions, check_module};
use music_syntax::{Lexer, parse};

use crate::{IrCasePattern, IrExprKind};
use crate::lower_module;

fn lower(src: &str) -> crate::IrModule {
    let lexed = Lexer::new(src).lex();
    let parsed = parse(lexed);
    assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

    let mut interner = Interner::new();
    let resolved = resolve_module(
        SourceId::from_raw(1),
        &ModuleKey::new("main"),
        parsed.tree(),
        &mut interner,
        ResolveOptions::default(),
    );
    let sema = check_module(
        resolved,
        &mut interner,
        SemaOptions {
            target: None,
            env: None,
        },
    );
    lower_module(&sema, &interner).expect("ir lowering should succeed")
}

#[test]
fn lowers_exports_and_semantic_metadata() {
    let ir = lower(
        r"
        export let id[T] (x : T) : T := x;
        export let Console := effect {
          let readln () : String;
        };
        export let Eq[T] := class {
          let (=) (a : T, b : T) : Bool;
        };
        export instance[T] Eq[T] {
          let (=) (a : T, b : T) : Bool := true;
        };
    ",
    );

    assert!(ir.exported_value("id").is_some());
    assert_eq!(ir.callables.len(), 1);
    assert_eq!(ir.effects.len(), 1);
    assert_eq!(ir.classes.len(), 1);
    assert_eq!(ir.instances.len(), 1);
    assert!(ir.static_imports.is_empty());
}

#[test]
fn lowers_data_and_foreign_facts() {
    let ir = lower(
        r#"
        let Maybe := data { | Some : Int | None };
        foreign "c" (
          let puts (value : CString) : Int;
        );
        export let answer () : Int := 42;
    "#,
    );

    assert_eq!(ir.data_defs.len(), 1);
    assert_eq!(ir.data_defs[0].variant_count, 2);
    assert_eq!(ir.foreigns.len(), 1);
    assert_eq!(ir.foreigns[0].abi.as_ref(), "c");
    assert_eq!(ir.foreigns[0].param_count, 1);
    assert!(ir.foreigns[0].link.is_none());
    assert_eq!(ir.callables.len(), 1);
    assert_eq!(ir.exports.len(), 1);
}

#[test]
fn lowers_array_cat_for_runtime_spread() {
    let ir = lower(
        r"
        let xs := [1, 2];
        export let ys := [0, ...xs, 3];
    ",
    );
    let ys = ir
        .globals
        .iter()
        .find(|global| global.name.as_ref() == "ys")
        .expect("ys global");
    let IrExprKind::Sequence { exprs } = &ys.body.kind else {
        panic!("expected sequence");
    };
    let Some(last) = exprs.last() else {
        panic!("expected sequence tail");
    };
    assert!(matches!(last.kind, IrExprKind::ArrayCat { .. }));
}

#[test]
fn lowers_call_seq_for_runtime_any_spread() {
    let ir = lower(
        r#"
        let g (a : Any, b : Any) : Any := a;
        let xs : Array[Any] := [1, "x"];
        export let y := g(...xs);
    "#,
    );
    let y = ir
        .globals
        .iter()
        .find(|global| global.name.as_ref() == "y")
        .expect("y global");
    let ok = match &y.body.kind {
        IrExprKind::Sequence { exprs } => exprs
            .last()
            .is_some_and(|expr| matches!(expr.kind, IrExprKind::CallSeq { .. })),
        kind => matches!(kind, IrExprKind::CallSeq { .. }),
    };
    assert!(ok, "expected call seq");
}

#[test]
fn lowers_call_with_compile_time_tuple_spread() {
    let ir = lower(
        r#"
        let f (a : Int, b : String) : Int := a;
        let t := (1, "x");
        export let y := f(...t);
    "#,
    );
    let y = ir
        .globals
        .iter()
        .find(|global| global.name.as_ref() == "y")
        .expect("y global");
    let IrExprKind::Sequence { exprs } = &y.body.kind else {
        panic!("expected sequence");
    };
    let Some(last) = exprs.last() else {
        panic!("expected sequence tail");
    };
    assert!(matches!(last.kind, IrExprKind::Call { .. }));
}

#[test]
fn lowers_perform_seq_for_runtime_any_spread() {
    let ir = lower(
        r#"
        let E := effect {
          let op (a : Any, b : Any) : Unit;
        };
        let xs : Array[Any] := [1, "x"];
        export let y := perform E.op(...xs);
    "#,
    );
    let y = ir
        .globals
        .iter()
        .find(|global| global.name.as_ref() == "y")
        .expect("y global");
    let ok = match &y.body.kind {
        IrExprKind::Sequence { exprs } => exprs
            .last()
            .is_some_and(|expr| matches!(expr.kind, IrExprKind::PerformSeq { .. })),
        kind => matches!(kind, IrExprKind::PerformSeq { .. }),
    };
    assert!(ok, "expected perform seq");
}

#[test]
fn lowers_sum_constructors_as_synthetic_variants() {
    let ir = lower(
        r#"
        export let x : Int + String := .Left(1);
        export let y : Int + String := .Right("x");
        export let z (v : Int + String) : Int := case v of (
          | .Left(n) => n
          | .Right(_) => 0
        );
    "#,
    );

    let synth = ir
        .data_defs
        .iter()
        .find(|data| data.key.name.starts_with("__sum__"))
        .expect("synthetic sum data def");
    assert_eq!(synth.variant_count, 2);

    let x = ir
        .globals
        .iter()
        .find(|global| global.name.as_ref() == "x")
        .expect("x global");
    let IrExprKind::VariantNew { data_key, .. } = &x.body.kind else {
        panic!("expected variant new");
    };
    assert_eq!(data_key.name.as_ref(), synth.key.name.as_ref());

    let z = ir
        .callables
        .iter()
        .find(|callable| callable.name.as_ref() == "z")
        .expect("z callable");
    let IrExprKind::Case { arms, .. } = &z.body.kind else {
        panic!("expected case");
    };
    let Some(arm) = arms.first() else {
        panic!("expected at least one arm");
    };
    let IrCasePattern::Variant { data_key, .. } = &arm.pattern else {
        panic!("expected variant pattern");
    };
    assert_eq!(data_key.name.as_ref(), synth.key.name.as_ref());
}

#[test]
fn lowers_type_test_and_cast() {
    let ir = lower(
        r"
        export let check (x : Any) : Bool := x :? Int;
        export let cast (x : Any) : Int := x :?> Int;
    ",
    );

    let check = ir
        .callables
        .iter()
        .find(|callable| callable.name.as_ref() == "check")
        .expect("check callable");
    let check_kind = match &check.body.kind {
        IrExprKind::Sequence { exprs } => &exprs.last().expect("sequence tail").kind,
        kind => kind,
    };
    assert!(matches!(check_kind, IrExprKind::TyTest { .. }));

    let cast = ir
        .callables
        .iter()
        .find(|callable| callable.name.as_ref() == "cast")
        .expect("cast callable");
    let cast_kind = match &cast.body.kind {
        IrExprKind::Sequence { exprs } => &exprs.last().expect("sequence tail").kind,
        kind => kind,
    };
    assert!(matches!(cast_kind, IrExprKind::TyCast { .. }));
}
