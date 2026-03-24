use music_ast::common::{FnDecl, MemberName, ModifierSet, TyRef};
use music_ast::data::AstData;
use music_ast::expr::{BinOp, ExprKind, FStrPart, IndexKind, LetBinding, MatchArm, RecordField};
use music_ast::pat::PatKind;
use music_db::Db;
use music_found::{Ident, Interner, Literal, SourceMap, Span, Spanned};
use music_hir::HirBundle;
use music_il::instruction::{Instruction, Operand};
use music_il::opcode::Opcode;
use music_resolve::queries::ResolutionMap;
use music_sema::env::TypeEnv;

use crate::emitter::emit;

fn build_thir(builders: &[fn(&mut AstData, &mut Interner) -> ExprKind]) -> HirBundle {
    let mut interner = Interner::new();
    let mut ast = AstData::new();

    for builder in builders {
        let kind = builder(&mut ast, &mut interner);
        let expr_id = ast.exprs.alloc(Spanned::dummy(kind));
        ast.root.push(expr_id);
    }

    let db = Db::new(ast, interner, SourceMap::default());
    let resolution = ResolutionMap::new();
    let type_env = TypeEnv::new();
    HirBundle::new(db, resolution, type_env)
}

fn build_thir_single(builder: fn(&mut AstData, &mut Interner) -> ExprKind) -> HirBundle {
    build_thir(&[builder])
}

#[test]
fn emit_literal_zero() {
    let thir = build_thir_single(|_ast, _int| ExprKind::Lit(Literal::Int(0)));
    let module = emit(&thir);
    assert_eq!(module.methods.len(), 1);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdZero));
}

#[test]
fn emit_literal_one() {
    let thir = build_thir_single(|_ast, _int| ExprKind::Lit(Literal::Int(1)));
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdOne));
}

#[test]
fn emit_literal_smi() {
    let thir = build_thir_single(|_ast, _int| ExprKind::Lit(Literal::Int(42)));
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 42));
}

#[test]
fn emit_literal_large_int_uses_constant_pool() {
    let thir = build_thir_single(|_ast, _int| ExprKind::Lit(Literal::Int(100_000)));
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0].opcode, Opcode::LdCst);
    assert!(matches!(instrs[0].operand, Operand::U16(0)));
    assert_eq!(module.constants.len(), 1);
}

#[test]
fn emit_literal_float() {
    let thir = build_thir_single(|_ast, _int| ExprKind::Lit(Literal::Float(1.234_567_89)));
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0].opcode, Opcode::LdCst);
    assert_eq!(module.constants.len(), 1);
}

#[test]
fn emit_literal_string() {
    let thir = build_thir_single(|_ast, _int| ExprKind::Lit(Literal::Str(String::from("hi"))));
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0].opcode, Opcode::LdCst);
    assert_eq!(module.constants.len(), 1);
}

#[test]
fn emit_literal_rune_small() {
    let thir = build_thir_single(|_ast, _int| ExprKind::Lit(Literal::Rune('a')));
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 97));
}

#[test]
fn emit_addition() {
    let thir = build_thir_single(|ast, _int| {
        let lhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let rhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        ExprKind::BinOp(BinOp::Add, lhs, rhs)
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[1], Instruction::with_i16(Opcode::LdSmi, 2));
    assert_eq!(instrs[2], Instruction::simple(Opcode::IAdd));
}

#[test]
fn emit_let_binding() {
    let thir = build_thir_single(|ast, interner| {
        let x_sym = interner.intern("x");
        let pat_id = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(x_sym))));
        let value = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(42))));
        let let_expr = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Let(Box::new(LetBinding {
                modifiers: ModifierSet::default(),
                attrs: Vec::new(),
                pat: pat_id,
                sig: None,
                value: Some(value),
            }))));
        let var_x = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Var(Ident::dummy(x_sym))));
        ExprKind::Seq(vec![let_expr, var_x])
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    // [ld.smi 42, st.loc 0, pop, ld.loc 0, halt]
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 42));
    assert_eq!(instrs[1], Instruction::with_u8(Opcode::StLoc, 0));
}

#[test]
fn emit_branch() {
    let thir = build_thir_single(|ast, _int| {
        let cond = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let then_br = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        let else_br = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(3))));
        ExprKind::Branch {
            cond,
            then_br,
            else_br,
        }
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[1].opcode, Opcode::BrFalse);
    assert_eq!(instrs[2], Instruction::with_i16(Opcode::LdSmi, 2));
    assert_eq!(instrs[3].opcode, Opcode::BrJmp);
    assert_eq!(instrs[4], Instruction::with_i16(Opcode::LdSmi, 3));
}

#[test]
fn emit_sequence_pops_intermediates() {
    let thir = build_thir_single(|ast, _int| {
        let a = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let b = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        ExprKind::Seq(vec![a, b])
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[1], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[2], Instruction::with_i16(Opcode::LdSmi, 2));
}

#[test]
fn emit_record_lit() {
    let thir = build_thir_single(|ast, interner| {
        let x_sym = interner.intern("x");
        let val = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let field = RecordField::Named {
            name: Ident::new(x_sym, Span::DUMMY),
            value: Some(val),
        };
        ExprKind::RecordLit(vec![field])
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_u16(Opcode::ArrNew, 1));
    assert_eq!(instrs[1], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[2], Instruction::with_u8(Opcode::ArrSeti, 0));
}

#[test]
fn emit_return_with_value() {
    let thir = build_thir_single(|ast, _int| {
        let val = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(5))));
        ExprKind::Return(Some(val))
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 5));
    assert_eq!(instrs[1], Instruction::simple(Opcode::Ret));
}

#[test]
fn emit_return_unit() {
    let thir = build_thir_single(|_ast, _int| ExprKind::Return(None));
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdUnit));
    assert_eq!(instrs[1], Instruction::simple(Opcode::Ret));
}

#[test]
fn emit_empty_module_produces_no_methods() {
    let thir = build_thir(&[]);
    let module = emit(&thir);
    assert!(module.methods.is_empty());
}

#[test]
fn emit_negative_smi() {
    let thir = build_thir_single(|_ast, _int| ExprKind::Lit(Literal::Int(-100)));
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, -100));
}

#[test]
fn emit_array_lit() {
    let thir = build_thir_single(|ast, _int| {
        let a = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let b = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        ExprKind::ArrayLit(vec![a, b])
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_u16(Opcode::ArrNew, 2));
    assert_eq!(instrs[1], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[2], Instruction::with_u8(Opcode::ArrSeti, 0));
    assert_eq!(instrs[3], Instruction::with_i16(Opcode::LdSmi, 2));
    assert_eq!(instrs[4], Instruction::with_u8(Opcode::ArrSeti, 1));
}

#[test]
fn emit_index_single() {
    let thir = build_thir_single(|ast, _int| {
        let arr = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));
        let idx = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        ExprKind::Index {
            expr: arr,
            indices: vec![idx],
            kind: IndexKind::Point,
        }
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdZero));
    assert_eq!(instrs[1], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[2], Instruction::simple(Opcode::ArrGet));
}

#[test]
fn emit_index_chained() {
    let thir = build_thir_single(|ast, _int| {
        let arr = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));
        let i = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let j = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        ExprKind::Index {
            expr: arr,
            indices: vec![i, j],
            kind: IndexKind::Point,
        }
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    // arr, i, ArrGet, j, ArrGet
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdZero));
    assert_eq!(instrs[1], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[2], Instruction::simple(Opcode::ArrGet));
    assert_eq!(instrs[3], Instruction::with_i16(Opcode::LdSmi, 2));
    assert_eq!(instrs[4], Instruction::simple(Opcode::ArrGet));
}

#[test]
fn emit_fstr_single_lit() {
    let thir = build_thir_single(|_ast, _int| {
        ExprKind::FStrLit(vec![FStrPart::Lit(String::from("hello"))])
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0].opcode, Opcode::LdCst);
    assert_eq!(module.constants.len(), 1);
}

#[test]
fn emit_fstr_multiple_parts() {
    let thir = build_thir_single(|ast, _int| {
        let expr = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(42))));
        ExprKind::FStrLit(vec![
            FStrPart::Lit(String::from("x is ")),
            FStrPart::Expr(expr),
        ])
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    // LdCst "x is ", LdSmi 42, ArrConcat
    assert_eq!(instrs[0].opcode, Opcode::LdCst);
    assert_eq!(instrs[1], Instruction::with_i16(Opcode::LdSmi, 42));
    assert_eq!(instrs[2], Instruction::simple(Opcode::ArrConcat));
}

#[test]
fn emit_range_inclusive() {
    let thir = build_thir_single(|ast, _int| {
        let lhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let rhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(10))));
        ExprKind::BinOp(BinOp::Range, lhs, rhs)
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    // ArrNewt("Range", 2), LdOne, ArrSeti(0), LdSmi(10), ArrSeti(1)
    assert_eq!(instrs[0].opcode, Opcode::ArrNewt);
    assert!(matches!(instrs[0].operand, Operand::Tagged(_, 2)));
    assert_eq!(instrs[1], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[2], Instruction::with_u8(Opcode::ArrSeti, 0));
    assert_eq!(instrs[3], Instruction::with_i16(Opcode::LdSmi, 10));
    assert_eq!(instrs[4], Instruction::with_u8(Opcode::ArrSeti, 1));
}

#[test]
fn emit_range_exclusive() {
    let thir = build_thir_single(|ast, _int| {
        let lhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));
        let rhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(5))));
        ExprKind::BinOp(BinOp::RangeExcl, lhs, rhs)
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0].opcode, Opcode::ArrNewt);
    assert!(matches!(instrs[0].operand, Operand::Tagged(_, 2)));
    assert_eq!(instrs[1], Instruction::simple(Opcode::LdZero));
    assert_eq!(instrs[2], Instruction::with_u8(Opcode::ArrSeti, 0));
    assert_eq!(instrs[3], Instruction::with_i16(Opcode::LdSmi, 5));
    assert_eq!(instrs[4], Instruction::with_u8(Opcode::ArrSeti, 1));
}

#[test]
fn emit_need() {
    let thir = build_thir_single(|ast, _int| {
        let operand = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(7))));
        ExprKind::Need(operand)
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 7));
    assert_eq!(instrs[1], Instruction::simple(Opcode::EffNeed));
}

#[test]
fn emit_handle_with_body() {
    let thir = build_thir_single(|ast, interner| {
        let body_expr = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let handler_body = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        let eff_sym = interner.intern("MyEff");
        let handler = FnDecl {
            name: MemberName::Ident(Ident::dummy(eff_sym)),
            params: None,
            ret_ty: None,
            body: Some(handler_body),
        };
        ExprKind::Handle {
            effect: TyRef {
                name: Ident::dummy(eff_sym),
                args: Vec::new(),
            },
            handlers: vec![handler],
            body: body_expr,
        }
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    // EffPush, handler body (LdSmi 2), body (LdOne), EffPop
    assert_eq!(instrs[0], Instruction::simple(Opcode::EffPush));
    assert_eq!(instrs[1], Instruction::with_i16(Opcode::LdSmi, 2));
    assert_eq!(instrs[2], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[3], Instruction::simple(Opcode::EffPop));
}

#[test]
fn emit_resume_with_value() {
    let thir = build_thir_single(|ast, _int| {
        let val = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(3))));
        ExprKind::Resume(Some(val))
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 3));
    assert_eq!(instrs[1], Instruction::simple(Opcode::EffResume));
}

#[test]
fn emit_resume_unit() {
    let thir = build_thir_single(|_ast, _int| ExprKind::Resume(None));
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdUnit));
    assert_eq!(instrs[1], Instruction::simple(Opcode::EffResume));
}

#[test]
fn emit_match_literal_pattern() {
    let thir = build_thir_single(|ast, _int| {
        let scrutinee = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(5))));
        let pat_lit = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Lit(Literal::Int(1))));
        let body1 = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(10))));
        let pat_wild = ast.pats.alloc(Spanned::dummy(PatKind::Wildcard));
        let body2 = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(20))));
        ExprKind::Match(
            scrutinee,
            vec![
                MatchArm {
                    attrs: Vec::new(),
                    pat: pat_lit,
                    guard: None,
                    body: body1,
                },
                MatchArm {
                    attrs: Vec::new(),
                    pat: pat_wild,
                    guard: None,
                    body: body2,
                },
            ],
        )
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    // scrutinee(LdSmi 5), Dup, LdOne(lit 1), CmpEq, BrFalse, Pop, body1(LdSmi 10), BrJmp, Pop, body2(LdSmi 20)
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 5));
    assert_eq!(instrs[1], Instruction::simple(Opcode::Dup));
    assert_eq!(instrs[2], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[3], Instruction::simple(Opcode::CmpEq));
    assert_eq!(instrs[4].opcode, Opcode::BrFalse);
    assert_eq!(instrs[5], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[6], Instruction::with_i16(Opcode::LdSmi, 10));
    assert_eq!(instrs[7].opcode, Opcode::BrJmp);
    assert_eq!(instrs[8], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[9], Instruction::with_i16(Opcode::LdSmi, 20));
}

#[test]
fn emit_match_tuple_destructure() {
    let thir = build_thir_single(|ast, interner| {
        let a_sym = interner.intern("a");
        let b_sym = interner.intern("b");
        let scrutinee = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));
        let pat_a = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(a_sym))));
        let pat_b = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(b_sym))));
        let tuple_pat = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Tuple(vec![pat_a, pat_b])));
        let var_a = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Var(Ident::dummy(a_sym))));
        ExprKind::Match(
            scrutinee,
            vec![MatchArm {
                attrs: Vec::new(),
                pat: tuple_pat,
                guard: None,
                body: var_a,
            }],
        )
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    // scrutinee(LdZero), Dup, ArrGeti(0), StLoc(0), Dup, ArrGeti(1), StLoc(1), Pop, body(LdLoc 0)
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdZero));
    assert_eq!(instrs[1], Instruction::simple(Opcode::Dup));
    assert_eq!(instrs[2], Instruction::with_u8(Opcode::ArrGeti, 0));
    assert_eq!(instrs[3], Instruction::with_u8(Opcode::StLoc, 0));
    assert_eq!(instrs[4], Instruction::simple(Opcode::Dup));
    assert_eq!(instrs[5], Instruction::with_u8(Opcode::ArrGeti, 1));
    assert_eq!(instrs[6], Instruction::with_u8(Opcode::StLoc, 1));
    assert_eq!(instrs[7], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[8], Instruction::with_u8(Opcode::LdLoc, 0));
}

#[test]
fn emit_match_with_guard() {
    let thir = build_thir_single(|ast, interner| {
        let x_sym = interner.intern("x");
        let scrutinee = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(5))));
        let pat_bind = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(x_sym))));
        let guard_expr = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let body1 = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(10))));
        let pat_wild = ast.pats.alloc(Spanned::dummy(PatKind::Wildcard));
        let body2 = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(20))));
        ExprKind::Match(
            scrutinee,
            vec![
                MatchArm {
                    attrs: Vec::new(),
                    pat: pat_bind,
                    guard: Some(guard_expr),
                    body: body1,
                },
                MatchArm {
                    attrs: Vec::new(),
                    pat: pat_wild,
                    guard: None,
                    body: body2,
                },
            ],
        )
    });
    let module = emit(&thir);
    let instrs = &module.methods[0].instructions;
    // scrutinee(LdSmi 5), StLoc(0), guard(LdOne), BrFalse, body1(LdSmi 10), BrJmp, Pop, body2(LdSmi 20)
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 5));
    assert_eq!(instrs[1], Instruction::with_u8(Opcode::StLoc, 0));
    assert_eq!(instrs[2], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[3].opcode, Opcode::BrFalse);
    assert_eq!(instrs[4], Instruction::with_i16(Opcode::LdSmi, 10));
    assert_eq!(instrs[5].opcode, Opcode::BrJmp);
    assert_eq!(instrs[6], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[7], Instruction::with_i16(Opcode::LdSmi, 20));
}

#[test]
fn emit_lambda_with_upvalue_capture() {
    // Build: let x := 42; let f := () => x
    // The lambda captures x from the outer scope.
    let mut interner = Interner::new();
    let mut ast = AstData::new();

    let x_sym = interner.intern("x");

    // let x := 42
    let pat_x = ast
        .pats
        .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(x_sym))));
    let lit_42 = ast
        .exprs
        .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(42))));
    let let_x = ast
        .exprs
        .alloc(Spanned::dummy(ExprKind::Let(Box::new(LetBinding {
            modifiers: ModifierSet::default(),
            attrs: Vec::new(),
            pat: pat_x,
            sig: None,
            value: Some(lit_42),
        }))));

    // Lambda body: x (a Var reference)
    let var_x = ast
        .exprs
        .alloc(Spanned::dummy(ExprKind::Var(Ident::dummy(x_sym))));

    // The lambda expression: () => x
    let lambda_expr = ast.exprs.alloc(Spanned::dummy(ExprKind::Lambda {
        params: Vec::new(),
        ret_ty: None,
        body: var_x,
    }));

    // Top-level sequence: let x := 42; (() => x)
    let seq = ast
        .exprs
        .alloc(Spanned::dummy(ExprKind::Seq(vec![let_x, lambda_expr])));
    ast.root.push(seq);

    let db = Db::new(ast, interner, SourceMap::default());
    let mut resolution = ResolutionMap::new();
    // Manually populate captures: the lambda captures x
    let _ = resolution.captures.insert(lambda_expr, vec![x_sym]);
    let type_env = TypeEnv::new();
    let thir = HirBundle::new(db, resolution, type_env);

    let module = emit(&thir);

    // Main method: the sequence
    let main = &module.methods.last().unwrap();
    let instrs = &main.instructions;

    // Expected sequence in main:
    // LdSmi(42), StLoc(0),  -- let x := 42
    // Pop,                   -- seq intermediate pop
    // LdLoc(0),              -- push captured x onto stack before ClsNew
    // ClsNew(0, 1),          -- create closure with 1 upvalue
    // Halt
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 42));
    assert_eq!(instrs[1], Instruction::with_u8(Opcode::StLoc, 0));
    assert_eq!(instrs[2], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[3], Instruction::with_u8(Opcode::LdLoc, 0));
    assert_eq!(instrs[4], Instruction::with_wide(Opcode::ClsNew, 0, 1));

    // The lambda body (method 0): should use LdUpv(0) to access x
    let lambda_method = &module.methods[0];
    let lambda_instrs = &lambda_method.instructions;
    assert_eq!(lambda_instrs[0], Instruction::with_u8(Opcode::LdUpv, 0));
    assert_eq!(lambda_instrs[1], Instruction::simple(Opcode::Ret));
}
