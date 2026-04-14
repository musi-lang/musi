use std::collections::BTreeMap;

use musi_foundation::core::{MODULE as CORE_MODULE, SPEC as CORE_SPEC};
use music_base::SourceId;
use music_module::{
    ImportEnv, ImportError, ImportErrorKind, ImportResolveResult, ModuleKey, ModuleSpecifier,
};
use music_names::Interner;
use music_resolve::{ResolveOptions, resolve_module};
use music_sema::{ModuleSurface, SemaEnv, SemaOptions, check_module};
use music_syntax::{Lexer, parse};

use crate::{
    IrArg, IrAssignTarget, IrBinaryOp, IrCallable, IrCasePattern, IrExpr, IrExprKind, IrMatchArm,
    IrModule, IrSeqPart, lower_module,
};

#[derive(Default)]
struct TestImportEnv {
    modules: BTreeMap<String, ModuleKey>,
}

impl TestImportEnv {
    fn with_module(mut self, spec: &str, key: &str) -> Self {
        let _prev = self.modules.insert(spec.into(), ModuleKey::new(key));
        self
    }
}

impl ImportEnv for TestImportEnv {
    fn resolve(&self, _from: &ModuleKey, spec: &ModuleSpecifier) -> ImportResolveResult {
        self.modules
            .get(spec.as_str())
            .cloned()
            .ok_or_else(|| ImportError::new(ImportErrorKind::ModuleNotFound, spec.as_str()))
    }
}

#[derive(Default)]
struct TestSemaEnv {
    modules: BTreeMap<String, ModuleSurface>,
}

impl TestSemaEnv {
    fn with_surface(mut self, key: &str, surface: ModuleSurface) -> Self {
        let _prev = self.modules.insert(key.into(), surface);
        self
    }
}

impl SemaEnv for TestSemaEnv {
    fn module_surface(&self, key: &ModuleKey) -> Option<ModuleSurface> {
        self.modules.get(key.as_str()).cloned()
    }
}

fn compile_surface(
    source_id: u32,
    module_key: &str,
    src: &str,
    import_env: Option<&dyn ImportEnv>,
    sema_env: Option<&dyn SemaEnv>,
) -> ModuleSurface {
    let lexed = Lexer::new(src).lex();
    let parsed = parse(lexed);
    assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

    let mut interner = Interner::new();
    let resolved = resolve_module(
        SourceId::from_raw(source_id),
        &ModuleKey::new(module_key),
        parsed.tree(),
        &mut interner,
        ResolveOptions {
            inject_compiler_prelude: true,
            prelude: Vec::new(),
            import_env,
        },
    );
    let sema = check_module(
        resolved,
        &mut interner,
        SemaOptions {
            target: None,
            env: sema_env,
            prelude: None,
        },
    );
    assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    sema.surface().clone()
}

fn lower(src: &str) -> IrModule {
    let lexed = Lexer::new(src).lex();
    let parsed = parse(lexed);
    assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

    let import_env = TestImportEnv::default().with_module(CORE_SPEC, CORE_SPEC);
    let core_surface = compile_surface(10, CORE_SPEC, CORE_MODULE, None, None);
    let sema_env = TestSemaEnv::default().with_surface(CORE_SPEC, core_surface);
    let mut interner = Interner::new();
    let resolved = resolve_module(
        SourceId::from_raw(1),
        &ModuleKey::new("main"),
        parsed.tree(),
        &mut interner,
        ResolveOptions {
            inject_compiler_prelude: true,
            prelude: Vec::new(),
            import_env: Some(&import_env),
        },
    );
    let sema = check_module(
        resolved,
        &mut interner,
        SemaOptions {
            target: None,
            env: Some(&sema_env),
            prelude: None,
        },
    );
    assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    lower_module(&sema, &interner).expect("ir lowering should succeed")
}

fn assert_global_tail_matches(
    src: &str,
    global_name: &str,
    predicate: impl FnOnce(&IrExprKind) -> bool,
) {
    let ir = lower(src);
    let global = ir
        .globals()
        .iter()
        .find(|item| item.name.as_ref() == global_name)
        .expect("global");
    let kind = match &global.body.kind {
        IrExprKind::Sequence { exprs } => &exprs.last().expect("sequence tail").kind,
        kind => kind,
    };
    assert!(predicate(kind), "unexpected global tail kind");
}

fn callable<'a>(ir: &'a IrModule, name: &str) -> &'a IrCallable {
    ir.callables()
        .iter()
        .find(|callable| callable.name.as_ref() == name)
        .expect("callable")
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
          let (=) (a : T, b : T) : Bool := 0 = 0;
        };
    ",
    );

    assert!(ir.exported_value("id").is_some());
    assert!(!ir.callables().is_empty());
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

    let maybe = ir
        .data_defs()
        .iter()
        .find(|data| data.key.name.as_ref() == "Maybe")
        .expect("Maybe data def");
    assert_eq!(maybe.variant_count, 2);
    assert_eq!(maybe.variants.len(), 2);
    let some_variant = maybe
        .variants
        .iter()
        .find(|variant| variant.name.as_ref() == "Some")
        .expect("Some variant");
    assert_eq!(some_variant.field_tys[0].as_ref(), "Int");
    assert_eq!(ir.foreigns().len(), 1);
    assert_eq!(ir.foreigns()[0].abi.as_ref(), "c");
    assert_eq!(ir.foreigns()[0].param_tys.len(), 1);
    assert_eq!(ir.foreigns()[0].param_tys[0].as_ref(), "CString");
    assert_eq!(ir.foreigns()[0].result_ty.as_ref(), "Int");
    assert!(ir.foreigns()[0].link.is_none());
    assert!(!ir.callables().is_empty());
    assert_eq!(ir.exports().len(), 1);
}

#[test]
fn lowers_array_cat_for_runtime_spread() {
    assert_global_tail_matches(
        r"
        let xs := [1, 2];
        export let ys := [0, ...xs, 3];
    ",
        "ys",
        |kind| matches!(kind, IrExprKind::ArrayCat { .. }),
    );
}

#[test]
fn lowers_range_and_membership_exprs() {
    assert_global_tail_matches(
        r#"
        let Core := import "musi:core";
        let Range := Core.Range;
        let Rangeable := Core.Rangeable;
        export let xs := 1 ..< 4;
    "#,
        "xs",
        |kind| matches!(kind, IrExprKind::Range { .. }),
    );
    assert_global_tail_matches(
        r#"
        let Core := import "musi:core";
        let Bool := Core.Bool;
        let Rangeable := Core.Rangeable;
        let xs := 1 ..< 4;
        export let ok : Bool := 2 in xs;
    "#,
        "ok",
        |kind| matches!(kind, IrExprKind::RangeContains { .. }),
    );
}

#[test]
fn lowers_call_seq_for_runtime_any_spread() {
    assert_global_tail_matches(
        r#"
        let g (a : Any, b : Any) : Any := a;
        let xs : []Any := [1, "x"];
        export let y := g(...xs);
    "#,
        "y",
        |kind| matches!(kind, IrExprKind::CallSeq { .. }),
    );
}

#[test]
fn lowers_call_with_compile_time_tuple_spread() {
    assert_global_tail_matches(
        r#"
        let f (a : Int, b : String) : Int := a;
        let t := (1, "x");
        export let y := f(...t);
    "#,
        "y",
        |kind| matches!(kind, IrExprKind::Call { .. }),
    );
}

#[test]
fn lowers_perform_seq_for_runtime_any_spread() {
    assert_global_tail_matches(
        r#"
        let E := effect {
          let op (a : Any, b : Any) : Unit;
        };
        let xs : []Any := [1, "x"];
        export let y := request E.op(...xs);
    "#,
        "y",
        |kind| matches!(kind, IrExprKind::RequestSeq { .. }),
    );
}

#[test]
fn lowers_sum_constructors_as_synthetic_variants() {
    let ir = lower(
        r#"
        export let x : Int + String := .Left(1);
        export let y : Int + String := .Right("x");
        export let z (v : Int + String) : Int := match v (
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
    assert_eq!(synth.variants.len(), 2);

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
    let IrExprKind::Match { arms, .. } = &z.body.kind else {
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
fn lowers_template_literal_with_interpolation() {
    let ir = lower(
        r"
        export let msg (name : String) : String := `hello ${name}`;
    ",
    );

    let msg = ir
        .callables()
        .iter()
        .find(|callable| callable.name.as_ref() == "msg")
        .expect("msg callable");
    assert!(contains_strcat(&msg.body));
}

#[test]
fn lowers_prefix_ops() {
    let ir = lower(
        r"
        export let neg (x : Int) : Int := -x;
        export let inv (x : Bool) : Bool := not x;
    ",
    );

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
}

#[test]
fn lowers_record_case_and_capturing_rec() {
    let ir = lower(
        r"
        export let answer (n : Int) : Int := (
          let base := 1;
          let rec loop (x : Int) : Int := match x (| 0 => base | _ => loop(x - 1));
          let point := { x := 1, y := 2 };
          let picked : Int := match point (| { x } => x | _ => 0);
          picked + loop(n)
        );
    ",
    );

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

#[test]
fn local_constrained_helper_prebinds_hidden_evidence() {
    let ir = lower(
        r"
        let Mark[T] := class { };
        let markInt := instance Mark[Int] { };
        let requireMark (x : Int) : Int where Int : Mark := x;
        let count (value : Int) : Int where Int : Mark := (
          let helper (y : Int) : Int := requireMark(y);
          helper(value)
        );
    ",
    );

    let helper = callable(&ir, "helper");
    assert!(
        contains_named_value_ref(&helper.body, "__dict__::main::Mark[Int]"),
        "helper callable: {helper:?}",
    );
}

#[test]
fn instance_member_helper_captures_provider_evidence() {
    let ir = lower(
        r"
        let Mark[T] := class { };
        let markInt := instance Mark[Int] { };
        let requireMark (x : Int) : Int where Int : Mark := x;
        let UsesMark := class {
          let useMark (x : Int) : Int;
        };
        instance UsesMark where Int : Mark {
          let useMark (x : Int) : Int := (
            let helper (y : Int) : Int where Int : Mark := requireMark(y);
            helper(x)
          );
        };
    ",
    );

    let helper = callable(&ir, "helper");
    assert!(
        contains_named_value_ref(&helper.body, "__dict__::main::Mark[Int]"),
        "helper callable: {helper:?}",
    );
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
        IrExprKind::Range { lower, upper, .. } => contains_strcat(lower) || contains_strcat(upper),
        IrExprKind::RangeContains {
            value,
            range,
            evidence,
        } => contains_strcat(value) || contains_strcat(range) || contains_strcat(evidence),
        IrExprKind::RangeMaterialize { range, evidence } => {
            contains_strcat(range) || contains_strcat(evidence)
        }
        IrExprKind::Binary { left, right, .. } => contains_strcat(left) || contains_strcat(right),
        IrExprKind::Call { callee, args } => {
            contains_strcat(callee) || args.iter().any(|arg| contains_strcat(&arg.expr))
        }
        IrExprKind::Match { scrutinee, arms } => {
            contains_strcat(scrutinee)
                || arms.iter().any(|arm| {
                    arm.guard.as_ref().is_some_and(contains_strcat) || contains_strcat(&arm.expr)
                })
        }
        _ => false,
    }
}

fn contains_named_value_ref(expr: &IrExpr, expected: &str) -> bool {
    contains_named_value_ref_kind(&expr.kind, expected)
}

fn contains_named_value_ref_kind(kind: &IrExprKind, expected: &str) -> bool {
    match kind {
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
        IrExprKind::Range { .. }
        | IrExprKind::RangeContains { .. }
        | IrExprKind::RangeMaterialize { .. } => {
            contains_named_value_ref_in_range_kind(kind, expected)
        }
        IrExprKind::Assign { target, value } => {
            contains_named_value_ref_in_target(target, expected)
                || contains_named_value_ref(value, expected)
        }
        IrExprKind::Index { base, indices } => {
            contains_named_value_ref_in_index(base, indices, expected)
        }
        IrExprKind::Tuple { items, .. }
        | IrExprKind::Array { items, .. }
        | IrExprKind::ClosureNew {
            captures: items, ..
        }
        | IrExprKind::Request { args: items, .. } => {
            contains_named_value_ref_in_exprs(items, expected)
        }
        IrExprKind::ArrayCat { parts, .. } | IrExprKind::CallSeq { args: parts, .. } => {
            contains_named_value_ref_in_seq_parts(parts, expected)
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
        IrExprKind::Match { scrutinee, arms } => {
            contains_named_value_ref_in_case(scrutinee, arms, expected)
        }
        IrExprKind::Call { callee, args } => {
            contains_named_value_ref_in_call(callee, args, expected)
        }
        IrExprKind::VariantNew { args, .. } => args
            .iter()
            .any(|expr| contains_named_value_ref(expr, expected)),
        IrExprKind::RequestSeq { args, .. } => {
            contains_named_value_ref_in_seq_parts(args, expected)
        }
        IrExprKind::HandlerLit { value, ops, .. } => {
            contains_named_value_ref(value, expected)
                || ops
                    .iter()
                    .any(|op| contains_named_value_ref(&op.closure, expected))
        }
        IrExprKind::Handle { handler, body, .. } => {
            contains_named_value_ref(handler, expected) || contains_named_value_ref(body, expected)
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

fn contains_named_value_ref_in_range_kind(kind: &IrExprKind, expected: &str) -> bool {
    match kind {
        IrExprKind::Range { lower, upper, .. } => {
            contains_named_value_ref(lower, expected) || contains_named_value_ref(upper, expected)
        }
        IrExprKind::RangeContains {
            value,
            range,
            evidence,
        } => {
            contains_named_value_ref(value, expected)
                || contains_named_value_ref(range, expected)
                || contains_named_value_ref(evidence, expected)
        }
        IrExprKind::RangeMaterialize { range, evidence } => {
            contains_named_value_ref(range, expected)
                || contains_named_value_ref(evidence, expected)
        }
        _ => false,
    }
}

fn contains_named_value_ref_in_index(base: &IrExpr, indices: &[IrExpr], expected: &str) -> bool {
    contains_named_value_ref(base, expected)
        || indices
            .iter()
            .any(|expr| contains_named_value_ref(expr, expected))
}

fn contains_named_value_ref_in_exprs(exprs: &[IrExpr], expected: &str) -> bool {
    exprs
        .iter()
        .any(|expr| contains_named_value_ref(expr, expected))
}

fn contains_named_value_ref_in_seq_parts(parts: &[IrSeqPart], expected: &str) -> bool {
    parts.iter().any(|part| match part {
        IrSeqPart::Expr(expr) | IrSeqPart::Spread(expr) => contains_named_value_ref(expr, expected),
    })
}

fn contains_named_value_ref_in_case(
    scrutinee: &IrExpr,
    arms: &[IrMatchArm],
    expected: &str,
) -> bool {
    contains_named_value_ref(scrutinee, expected)
        || arms.iter().any(|arm| {
            arm.guard
                .as_ref()
                .is_some_and(|guard| contains_named_value_ref(guard, expected))
                || contains_named_value_ref(&arm.expr, expected)
        })
}

fn contains_named_value_ref_in_call(callee: &IrExpr, args: &[IrArg], expected: &str) -> bool {
    contains_named_value_ref(callee, expected)
        || args
            .iter()
            .any(|arg| contains_named_value_ref(&arg.expr, expected))
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
        IrExprKind::Match { scrutinee, arms } => {
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
        IrExprKind::Match { scrutinee, arms } => {
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
