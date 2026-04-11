use music_base::SourceId;
use music_module::ModuleKey;
use music_names::Interner;
use music_resolve::{ResolveOptions, resolve_module};
use music_sema::{SemaOptions, check_module};
use music_syntax::{Lexer, parse};

use crate::{
    IrAssignTarget, IrBinaryOp, IrCasePattern, IrExpr, IrExprKind, IrModule, IrSeqPart,
    lower_module,
};

fn lower(src: &str) -> IrModule {
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
    assert_eq!(ir.callables().len(), 1);
    assert_eq!(ir.effects().len(), 1);
    assert_eq!(ir.effects()[0].ops.len(), 1);
    assert!(ir.effects()[0].ops[0].param_tys.is_empty());
    assert_eq!(ir.effects()[0].ops[0].result_ty.as_ref(), "String");
    assert_eq!(ir.classes().len(), 1);
    assert_eq!(ir.instances().len(), 1);
    assert!(ir.static_imports().is_empty());
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

    assert_eq!(ir.data_defs().len(), 1);
    assert_eq!(ir.data_defs()[0].variant_count, 2);
    assert_eq!(ir.foreigns().len(), 1);
    assert_eq!(ir.foreigns()[0].abi.as_ref(), "c");
    assert_eq!(ir.foreigns()[0].param_tys.len(), 1);
    assert_eq!(ir.foreigns()[0].param_tys[0].as_ref(), "CString");
    assert_eq!(ir.foreigns()[0].result_ty.as_ref(), "Int");
    assert!(ir.foreigns()[0].link.is_none());
    assert_eq!(ir.callables().len(), 1);
    assert_eq!(ir.exports().len(), 1);
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
        .globals()
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
        .globals()
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
        .globals()
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
        .globals()
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
        .data_defs()
        .iter()
        .find(|data| data.key.name.starts_with("__sum__"))
        .expect("synthetic sum data def");
    assert_eq!(synth.variant_count, 2);

    let x = ir
        .globals()
        .iter()
        .find(|global| global.name.as_ref() == "x")
        .expect("x global");
    let IrExprKind::VariantNew { data_key, .. } = &x.body.kind else {
        panic!("expected variant new");
    };
    assert_eq!(data_key.name.as_ref(), synth.key.name.as_ref());

    let z = ir
        .callables()
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
        .callables()
        .iter()
        .find(|callable| callable.name.as_ref() == "check")
        .expect("check callable");
    let check_kind = match &check.body.kind {
        IrExprKind::Sequence { exprs } => &exprs.last().expect("sequence tail").kind,
        kind => kind,
    };
    assert!(matches!(check_kind, IrExprKind::TyTest { .. }));

    let cast = ir
        .callables()
        .iter()
        .find(|callable| callable.name.as_ref() == "cast")
        .expect("cast callable");
    let cast_kind = match &cast.body.kind {
        IrExprKind::Sequence { exprs } => &exprs.last().expect("sequence tail").kind,
        kind => kind,
    };
    assert!(matches!(cast_kind, IrExprKind::TyCast { .. }));
}

#[test]
fn capitalized_local_name_stays_value_expr() {
    let ir = lower(
        r"
        export let answer () : Int := (
          let Result : Int := 41;
          Result + 1
        );
    ",
    );

    let answer = ir
        .callables()
        .iter()
        .find(|callable| callable.name.as_ref() == "answer")
        .expect("answer callable");
    assert!(
        contains_named_value_ref(&answer.body, "Result"),
        "capitalized local binding should lower as a value reference"
    );
}

#[test]
fn lowers_template_prefix_ops_record_case_and_capturing_rec() {
    let ir = lower(
        r"
        export let msg (name : String) : String := `hello ${name}`;
        export let neg (x : Int) : Int := -x;
        export let inv (x : Bool) : Bool := not x;
        export let answer (n : Int) : Int := (
          let base := 1;
          let rec loop (x : Int) : Int := case x of (| 0 => base | _ => loop(x - 1));
          let point := { x := 1, y := 2 };
          let picked : Int := case point of (| { x } => x | _ => 0);
          picked + loop(n)
        );
    ",
    );

    let msg = ir
        .callables()
        .iter()
        .find(|callable| callable.name.as_ref() == "msg")
        .expect("msg callable");
    assert!(contains_strcat(&msg.body));

    let neg = ir
        .callables()
        .iter()
        .find(|callable| callable.name.as_ref() == "neg")
        .expect("neg callable");
    let neg_kind = match &neg.body.kind {
        IrExprKind::Sequence { exprs } => &exprs.last().expect("sequence tail").kind,
        kind => kind,
    };
    assert!(matches!(
        neg_kind,
        IrExprKind::Binary {
            op: IrBinaryOp::ISub,
            ..
        }
    ));

    let inv = ir
        .callables()
        .iter()
        .find(|callable| callable.name.as_ref() == "inv")
        .expect("inv callable");
    let inv_kind = match &inv.body.kind {
        IrExprKind::Sequence { exprs } => &exprs.last().expect("sequence tail").kind,
        kind => kind,
    };
    assert!(matches!(inv_kind, IrExprKind::Not { .. }));

    let answer = ir
        .callables()
        .iter()
        .find(|callable| callable.name.as_ref() == "answer")
        .expect("answer callable");
    assert!(contains_record_pattern(&answer.body));

    let loop_fn = ir
        .callables()
        .iter()
        .find(|callable| callable.name.as_ref() == "loop")
        .expect("loop callable");
    assert!(contains_closure_callee(&loop_fn.body));
}

fn contains_strcat(expr: &IrExpr) -> bool {
    match &expr.kind {
        IrExprKind::Binary {
            op: IrBinaryOp::StrCat,
            ..
        } => true,
        IrExprKind::Sequence { exprs } => exprs.iter().any(contains_strcat),
        IrExprKind::Let { value, .. }
        | IrExprKind::TempLet { value, .. }
        | IrExprKind::Not { expr: value }
        | IrExprKind::DynamicImport { spec: value }
        | IrExprKind::ModuleGet { base: value, .. }
        | IrExprKind::RecordGet { base: value, .. }
        | IrExprKind::TyTest { base: value, .. }
        | IrExprKind::TyCast { base: value, .. } => contains_strcat(value),
        IrExprKind::Binary { left, right, .. } => contains_strcat(left) || contains_strcat(right),
        IrExprKind::Call { callee, args } => {
            contains_strcat(callee) || args.iter().any(|arg| contains_strcat(&arg.expr))
        }
        IrExprKind::Case { scrutinee, arms } => {
            contains_strcat(scrutinee)
                || arms.iter().any(|arm| {
                    arm.guard.as_ref().is_some_and(contains_strcat) || contains_strcat(&arm.expr)
                })
        }
        _ => false,
    }
}

fn contains_named_value_ref(expr: &IrExpr, expected: &str) -> bool {
    match &expr.kind {
        IrExprKind::Name { name, .. } => name.as_ref() == expected,
        IrExprKind::Sequence { exprs } => exprs
            .iter()
            .any(|expr| contains_named_value_ref(expr, expected)),
        IrExprKind::Let { value, .. }
        | IrExprKind::TempLet { value, .. }
        | IrExprKind::Not { expr: value }
        | IrExprKind::DynamicImport { spec: value }
        | IrExprKind::ModuleGet { base: value, .. }
        | IrExprKind::RecordGet { base: value, .. }
        | IrExprKind::TyTest { base: value, .. }
        | IrExprKind::TyCast { base: value, .. } => contains_named_value_ref(value, expected),
        IrExprKind::Assign { target, value } => {
            contains_named_value_ref_in_target(target, expected)
                || contains_named_value_ref(value, expected)
        }
        IrExprKind::Index { base, indices } => {
            contains_named_value_ref(base, expected)
                || indices
                    .iter()
                    .any(|expr| contains_named_value_ref(expr, expected))
        }
        IrExprKind::Tuple { items, .. }
        | IrExprKind::Array { items, .. }
        | IrExprKind::ClosureNew {
            captures: items, ..
        }
        | IrExprKind::Perform { args: items, .. } => items
            .iter()
            .any(|expr| contains_named_value_ref(expr, expected)),
        IrExprKind::ArrayCat { parts, .. } | IrExprKind::CallSeq { args: parts, .. } => {
            parts.iter().any(|part| match part {
                IrSeqPart::Expr(expr) | IrSeqPart::Spread(expr) => {
                    contains_named_value_ref(expr, expected)
                }
            })
        }
        IrExprKind::Record { fields, .. } => fields
            .iter()
            .any(|field| contains_named_value_ref(&field.expr, expected)),
        IrExprKind::RecordUpdate { base, updates, .. } => {
            contains_named_value_ref(base, expected)
                || updates
                    .iter()
                    .any(|update| contains_named_value_ref(&update.expr, expected))
        }
        IrExprKind::Binary { left, right, .. } => {
            contains_named_value_ref(left, expected) || contains_named_value_ref(right, expected)
        }
        IrExprKind::Case { scrutinee, arms } => {
            contains_named_value_ref(scrutinee, expected)
                || arms.iter().any(|arm| {
                    arm.guard
                        .as_ref()
                        .is_some_and(|guard| contains_named_value_ref(guard, expected))
                        || contains_named_value_ref(&arm.expr, expected)
                })
        }
        IrExprKind::Call { callee, args } => {
            contains_named_value_ref(callee, expected)
                || args
                    .iter()
                    .any(|arg| contains_named_value_ref(&arg.expr, expected))
        }
        IrExprKind::VariantNew { args, .. } => args
            .iter()
            .any(|expr| contains_named_value_ref(expr, expected)),
        IrExprKind::PerformSeq { args, .. } => args.iter().any(|part| match part {
            IrSeqPart::Expr(expr) | IrSeqPart::Spread(expr) => {
                contains_named_value_ref(expr, expected)
            }
        }),
        IrExprKind::Handle {
            value, ops, body, ..
        } => {
            contains_named_value_ref(value, expected)
                || ops
                    .iter()
                    .any(|op| contains_named_value_ref(&op.closure, expected))
                || contains_named_value_ref(body, expected)
        }
        IrExprKind::Resume { expr } => expr
            .as_deref()
            .is_some_and(|expr| contains_named_value_ref(expr, expected)),
        IrExprKind::Unit
        | IrExprKind::Temp { .. }
        | IrExprKind::Lit(_)
        | IrExprKind::TypeValue { .. }
        | IrExprKind::SyntaxValue { .. } => false,
    }
}

fn contains_named_value_ref_in_target(target: &IrAssignTarget, expected: &str) -> bool {
    match target {
        IrAssignTarget::Binding { .. } => false,
        IrAssignTarget::Index { base, indices } => {
            contains_named_value_ref(base, expected)
                || indices
                    .iter()
                    .any(|expr| contains_named_value_ref(expr, expected))
        }
        IrAssignTarget::RecordField { base, .. } => contains_named_value_ref(base, expected),
    }
}

fn contains_record_pattern(expr: &IrExpr) -> bool {
    match &expr.kind {
        IrExprKind::Case { scrutinee, arms } => {
            contains_record_pattern(scrutinee)
                || arms.iter().any(|arm| {
                    matches!(arm.pattern, IrCasePattern::Record { .. })
                        || arm.guard.as_ref().is_some_and(contains_record_pattern)
                        || contains_record_pattern(&arm.expr)
                })
        }
        IrExprKind::Sequence { exprs } => exprs.iter().any(contains_record_pattern),
        IrExprKind::Let { value, .. } | IrExprKind::TempLet { value, .. } => {
            contains_record_pattern(value)
        }
        _ => false,
    }
}

fn contains_closure_callee(expr: &IrExpr) -> bool {
    match &expr.kind {
        IrExprKind::Call { callee, args } => {
            matches!(callee.kind, IrExprKind::ClosureNew { .. })
                || contains_closure_callee(callee)
                || args.iter().any(|arg| contains_closure_callee(&arg.expr))
        }
        IrExprKind::Sequence { exprs } => exprs.iter().any(contains_closure_callee),
        IrExprKind::Case { scrutinee, arms } => {
            contains_closure_callee(scrutinee)
                || arms.iter().any(|arm| {
                    arm.guard.as_ref().is_some_and(contains_closure_callee)
                        || contains_closure_callee(&arm.expr)
                })
        }
        IrExprKind::Let { value, .. } | IrExprKind::TempLet { value, .. } => {
            contains_closure_callee(value)
        }
        IrExprKind::Binary { left, right, .. } => {
            contains_closure_callee(left) || contains_closure_callee(right)
        }
        _ => false,
    }
}
