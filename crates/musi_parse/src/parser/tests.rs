use musi_lex::lex;
use musi_shared::{DiagnosticBag, FileId, Interner};

use super::*;
use crate::ast::{BinOp, Cond, Expr, Pat};

fn lex_and_parse(src: &str) -> (ParsedModule, DiagnosticBag) {
    let mut interner = Interner::new();
    let mut lex_diags = DiagnosticBag::new();
    let file_id = FileId(0);
    let lexed = lex(src, file_id, &mut interner, &mut lex_diags);
    let mut parse_diags = DiagnosticBag::new();
    let module = parse(&lexed.tokens, file_id, &mut parse_diags, &interner);
    // Merge lex errors into parse diags for uniform checking
    for d in lex_diags.iter() {
        parse_diags.push(d.clone());
    }
    (module, parse_diags)
}

fn parse_ok(src: &str) -> ParsedModule {
    let (module, diags) = lex_and_parse(src);
    assert!(
        !diags.has_errors(),
        "expected no errors for `{src}`, got: {:?}",
        diags.iter().collect::<Vec<_>>()
    );
    module
}

#[test]
fn parse_hello_world() {
    let module = parse_ok("writeln(\"Hello, world!\");");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(
            expr,
            Expr::Postfix {
                op: PostfixOp::Call { .. },
                ..
            }
        ),
        "expected a call expression, got: {expr:?}"
    );
}

// 2. fn add(a, b) => a + b -- FnDef with 2 params and body
#[test]
fn parse_fn_def() {
    let module = parse_ok("fn add(a, b) => a + b;");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(matches!(expr, Expr::FnDef { params, body: Some(_), .. } if params.len() == 2));
}

// 3. const x := 42 -- Bind
#[test]
fn parse_const_bind() {
    let module = parse_ok("const x := 42;");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(
            expr,
            Expr::Bind {
                kind: BindKind::Const,
                init: Some(_),
                ..
            }
        ),
        "expected Bind Const, got: {expr:?}"
    );
}

// 4. if true then (1) else (2) -- If with else
#[test]
fn parse_if_then_else() {
    let module = parse_ok("if true then (1) else (2);");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(
            expr,
            Expr::If {
                else_body: Some(_),
                ..
            }
        ),
        "expected If with else, got: {expr:?}"
    );
}

// 5. match with 2 arms
#[test]
fn parse_match_expr() {
    let module = parse_ok("match x with (0 => \"zero\" | _ => \"other\");");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(matches!(expr, Expr::Match { arms, .. } if arms.len() == 2));
}

// 6. choice Option { | Some(Int) | None }
#[test]
fn parse_choice_def() {
    let module = parse_ok("choice Option { | Some(Int) | None };");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(matches!(expr, Expr::Choice { variants, .. } if variants.len() == 2));
}

// 7. Broken input -- has_errors
#[test]
fn parse_broken_input_has_errors() {
    let (_module, diags) = lex_and_parse("fn foo(");
    assert!(diags.has_errors());
}

// 8. Binary operators produce correct BinOp variants
#[test]
fn parse_binary_ops() {
    let cases = [
        ("a + b", BinOp::Add),
        ("a - b", BinOp::Sub),
        ("a * b", BinOp::Mul),
        ("a / b", BinOp::Div),
        ("a % b", BinOp::Rem),
        ("a = b", BinOp::Eq),
        ("a /= b", BinOp::NotEq),
        ("a and b", BinOp::And),
        ("a or b", BinOp::Or),
    ];
    for (src, expected_op) in cases {
        let module = parse_ok(&format!("{src};"));
        let expr = module.ctx.exprs.get(module.items[0]);
        assert!(
            matches!(expr, Expr::Binary { op, .. } if *op == expected_op),
            "for `{src}`: expected {expected_op:?}, got: {expr:?}"
        );
    }
}

// 9. Pratt precedence: 1 + 2 * 3 -- Binary(Add, 1, Binary(Mul, 2, 3))
#[test]
fn pratt_precedence() {
    let module = parse_ok("1 + 2 * 3;");
    let top = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(top, Expr::Binary { op: BinOp::Add, .. }),
        "expected Add at top"
    );
    if let Expr::Binary { rhs, .. } = top {
        let rhs_expr = module.ctx.exprs.get(*rhs);
        assert!(
            matches!(rhs_expr, Expr::Binary { op: BinOp::Mul, .. }),
            "expected Mul on rhs, got: {rhs_expr:?}"
        );
    }
}

// 10. Lambda expression
#[test]
fn parse_lambda() {
    let module = parse_ok("fn (x) => x + 1;");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::Lambda { .. }),
        "expected Lambda, got: {expr:?}"
    );
}

// 11. Record definition
#[test]
fn parse_record_def() {
    let module = parse_ok("record Point { x: Int, y: Int };");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(matches!(expr, Expr::Record { fields, .. } if fields.len() == 2));
}

// 12. Array literal
#[test]
fn parse_array_literal() {
    let module = parse_ok("[1, 2, 3];");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(matches!(expr, Expr::Array { items, .. } if items.len() == 3));
}

// 13. Return expression
#[test]
fn parse_return_expr() {
    let module = parse_ok("return 42;");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::Return { value: Some(_), .. }),
        "expected Return with value, got: {expr:?}"
    );
}

// 14. Unit expression
#[test]
fn parse_unit() {
    let module = parse_ok("();");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::Unit { .. }),
        "expected Unit, got: {expr:?}"
    );
}

// 15. Tuple expression
#[test]
fn parse_tuple() {
    let module = parse_ok("(1, 2, 3);");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(matches!(expr, Expr::Tuple { elements, .. } if elements.len() == 3));
}

// 16. Block expression
#[test]
fn parse_block_expr() {
    let module = parse_ok("(const x := 1; x);");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::Block { stmts, tail: Some(_), .. } if stmts.len() == 1),
        "expected Block with 1 stmt and tail, got: {expr:?}"
    );
}

// 17. Prefix operators
#[test]
fn parse_prefix_neg() {
    let module = parse_ok("-42;");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(
            expr,
            Expr::Prefix {
                op: PrefixOp::Neg,
                ..
            }
        ),
        "expected Prefix Neg, got: {expr:?}"
    );
}

// 18. Assign expression
#[test]
fn parse_assign() {
    let module = parse_ok("x <- 42;");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::Assign { .. }),
        "expected Assign, got: {expr:?}"
    );
}

// 19. class Eq['T] { fn eq(a: 'T, b: 'T): Bool; }
#[test]
fn parse_class_def_simple() {
    let module = parse_ok("class Eq['T] { fn eq(a: 'T, b: 'T): Bool; };");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::ClassDef { members, supers, .. } if members.len() == 1 && supers.is_empty()),
        "expected ClassDef with 1 member and no supers, got: {expr:?}"
    );
}

// 20. class Ord['T] satisfies Eq['T] { fn lt(a: 'T, b: 'T): Bool; }
#[test]
fn parse_class_def_with_superclass() {
    let module = parse_ok("class Ord['T] satisfies Eq['T] { fn lt(a: 'T, b: 'T): Bool; };");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::ClassDef { supers, .. } if supers.len() == 1),
        "expected ClassDef with 1 super, got: {expr:?}"
    );
}

// 21. class with a law declaration
#[test]
fn parse_class_def_with_law() {
    let module = parse_ok("class Eq['T] { fn eq(a: 'T, b: 'T): Bool; law reflexivity(a: 'T) => eq(a, a); };");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::ClassDef { members, .. } if members.len() == 2),
        "expected ClassDef with 2 members (method + law), got: {expr:?}"
    );
}

// 22. given Eq[Int] { fn eq(a: Int, b: Int): Bool => a = b; }
#[test]
fn parse_given_def_simple() {
    let module = parse_ok("given Eq[Int] { fn eq(a: Int, b: Int): Bool => a = b; };");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::GivenDef { constraints, members, .. } if constraints.is_empty() && members.len() == 1),
        "expected GivenDef with no constraints and 1 member, got: {expr:?}"
    );
}

// 23. given Eq[Option['T]] where 'T satisfies Eq { fn eq(a: Option['T], b: Option['T]): Bool => true; }
#[test]
fn parse_given_def_with_where() {
    let module = parse_ok(
        "given Eq[Option['T]] where 'T satisfies Eq { fn eq(a: Option['T], b: Option['T]): Bool => true; };",
    );
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::GivenDef { constraints, .. } if constraints.len() == 1),
        "expected GivenDef with 1 constraint, got: {expr:?}"
    );
}

// 24. fn compare['T](a: 'T, b: 'T): Bool where 'T satisfies Ord => true
#[test]
fn parse_fn_with_where() {
    let module = parse_ok("fn compare['T](a: 'T, b: 'T): Bool where 'T satisfies Ord => true;");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::FnDef { where_clause, body: Some(_), .. } if where_clause.len() == 1),
        "expected FnDef with 1 where constraint and body, got: {expr:?}"
    );
}

// 25. UDN: ?Type sugar -> Ty::Option
#[test]
fn parse_option_type_sugar() {
    let module = parse_ok("fn f(x: ?Int): ?Int => x;");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    let Expr::FnDef { params, ret_ty, .. } = expr else { panic!("expected FnDef") };
    assert!(matches!(params[0].ty, Some(Ty::Option { .. })), "param type should be Ty::Option");
    assert!(matches!(ret_ty, Some(Ty::Option { .. })), "return type should be Ty::Option");
}

// 26. UDN: dot-prefix constructor .Name(args)
#[test]
fn parse_dot_prefix_call() {
    let module = parse_ok("const x := .Some(42);");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    let Expr::Bind { init: Some(init_idx), .. } = expr else { panic!("expected Bind") };
    let init = module.ctx.exprs.get(*init_idx);
    assert!(matches!(init, Expr::DotPrefix { args, .. } if args.len() == 1), "expected DotPrefix with 1 arg");
}

// 27. UDN: nil coalescing ??
#[test]
fn parse_nil_coalesce() {
    let module = parse_ok("const x := opt ?? 0;");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    let Expr::Bind { init: Some(init_idx), .. } = expr else { panic!("expected Bind") };
    let init = module.ctx.exprs.get(*init_idx);
    assert!(matches!(init, Expr::Binary { op: BinOp::NilCoalesce, .. }), "expected NilCoalesce");
}

// 28. UDN: optional chaining ?.
#[test]
fn parse_optional_chain() {
    let module = parse_ok("const x := opt?.value;");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    let Expr::Bind { init: Some(init_idx), .. } = expr else { panic!("expected Bind") };
    let init = module.ctx.exprs.get(*init_idx);
    assert!(matches!(init, Expr::Postfix { op: PostfixOp::OptField { .. }, .. }), "expected OptField");
}

// 29. Dot-prefix pattern in match arm
#[test]
fn parse_dot_prefix_pattern_in_match() {
    let module = parse_ok("match x with (.Some(v) => v | .None => 0);");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(matches!(expr, Expr::Match { .. }), "expected Match");
}

// 30. var prefix in case pattern
#[test]
fn parse_var_in_case_pattern() {
    let module = parse_ok("if case .Some(var v) := x then v;");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    let Expr::If { cond, .. } = expr else { panic!("expected If") };
    let Cond::Case { pat, .. } = cond.as_ref() else { panic!("expected Case cond") };
    let Pat::DotPrefix { args, .. } = pat else { panic!("expected DotPrefix in case") };
    assert_eq!(args.len(), 1);
    assert!(
        matches!(args[0], Pat::Ident { is_mut: true, .. }),
        "expected mutable binding inside DotPrefix, got: {:?}",
        args[0]
    );
}

// 31. match arm guard
#[test]
fn parse_match_arm_guard() {
    let module = parse_ok("match x with (Some(v) if v > 0 => v | _ => 0);");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    let Expr::Match { arms, .. } = expr else { panic!("expected Match") };
    assert!(arms[0].guard.is_some(), "first arm should have a guard");
    assert!(arms[1].guard.is_none(), "second arm should have no guard");
}

// 32. if case without const/var keyword
#[test]
fn parse_if_case_no_bind_kind() {
    let module = parse_ok("if case .Some(v) := thing then v else 0;");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    let Expr::If { cond, else_body, .. } = expr else { panic!("expected If") };
    assert!(matches!(cond.as_ref(), Cond::Case { .. }), "expected Case cond");
    assert!(else_body.is_some(), "expected else branch");
}
