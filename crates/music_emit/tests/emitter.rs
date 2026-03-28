#![allow(
    clippy::unwrap_used,
    clippy::panic,
    clippy::tests_outside_test_module,
    clippy::similar_names
)]

use music_ast::common::{FnDecl, MemberDecl, MemberName, ModifierSet, Param, Signature, TyRef};
use music_ast::data::AstData;
use music_ast::expr::{
    AccessMode, BinOp, CaseArm, CaseData, CompClause, ComprehensionData, ExprKind, FStrPart,
    FieldTarget, HandleData, HandlerClause, IndexKind, InstanceBody, InstanceDef, LetBinding,
    PostfixOp, RecordField, TypeOpKind, UnaryOp,
};
use music_ast::pat::{PatKind, RecordPatField};
use music_ast::ty::TyKind;
use music_db::Db;
use music_hir::TypedModule;
use music_il::format::{self, BUILTIN_TYPE_INT};
use music_il::instruction::{Instruction, Operand};
use music_il::opcode::Opcode;
use music_owned::types::BuiltinType;
use music_resolve::queries::ResolutionMap;
use music_sema::Ty;
use music_sema::env::{DispatchInfo, EffectUse, TypeEnv};
use music_shared::{Ident, Interner, Literal, SourceMap, Span, Spanned};

use music_emit::emitter::emit;

fn build_typed_module(builders: &[fn(&mut AstData, &mut Interner) -> ExprKind]) -> TypedModule {
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
    TypedModule::new(db, resolution, type_env)
}

fn build_typed_module_single(builder: fn(&mut AstData, &mut Interner) -> ExprKind) -> TypedModule {
    build_typed_module(&[builder])
}

#[test]
fn emit_literal_zero() {
    let typed_module = build_typed_module_single(|_ast, _int| ExprKind::Lit(Literal::Int(0)));
    let module = emit(&typed_module).unwrap();
    assert_eq!(module.methods.len(), 1);
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdNil));
}

#[test]
fn emit_literal_one() {
    let typed_module = build_typed_module_single(|_ast, _int| ExprKind::Lit(Literal::Int(1)));
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdOne));
}

#[test]
fn emit_literal_smi() {
    let typed_module = build_typed_module_single(|_ast, _int| ExprKind::Lit(Literal::Int(42)));
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 42));
}

#[test]
fn emit_literal_large_int_uses_constant_pool() {
    let typed_module = build_typed_module_single(|_ast, _int| ExprKind::Lit(Literal::Int(100_000)));
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0].opcode, Opcode::LdConst);
    assert!(matches!(instrs[0].operand, Operand::U16(0)));
    assert_eq!(module.constants.len(), 1);
}

#[test]
fn emit_literal_float() {
    let typed_module =
        build_typed_module_single(|_ast, _int| ExprKind::Lit(Literal::Float(1.234_567_89)));
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0].opcode, Opcode::LdConst);
    assert_eq!(module.constants.len(), 1);
}

#[test]
fn emit_literal_string() {
    let typed_module =
        build_typed_module_single(|_ast, _int| ExprKind::Lit(Literal::Str(String::from("hi"))));
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0].opcode, Opcode::LdConst);
    assert_eq!(module.constants.len(), 1);
}

#[test]
fn emit_literal_rune_small() {
    let typed_module = build_typed_module_single(|_ast, _int| ExprKind::Lit(Literal::Rune('a')));
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 97));
}

#[test]
fn emit_addition() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let lhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let rhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        ExprKind::BinOp(BinOp::Add, lhs, rhs)
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[1], Instruction::with_i16(Opcode::LdSmi, 2));
    assert_eq!(instrs[2], Instruction::simple(Opcode::IAdd));
}

#[test]
fn emit_let_binding() {
    let typed_module = build_typed_module_single(|ast, interner| {
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
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    // [ld.smi 42, st.loc 0, ld.unit, pop, ld.loc 0, halt]
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 42));
    assert_eq!(instrs[1], Instruction::with_u8(Opcode::StLoc, 0));
    assert_eq!(instrs[2], Instruction::simple(Opcode::LdUnit));
    assert_eq!(instrs[3], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[4], Instruction::with_u8(Opcode::LdLoc, 0));
}

#[test]
fn emit_branch() {
    let typed_module = build_typed_module_single(|ast, _int| {
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
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[1].opcode, Opcode::BrFalse);
    assert_eq!(instrs[2], Instruction::with_i16(Opcode::LdSmi, 2));
    assert_eq!(instrs[3].opcode, Opcode::BrJmp);
    assert_eq!(instrs[4], Instruction::with_i16(Opcode::LdSmi, 3));
}

#[test]
fn emit_branch_mixed_width() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let cond = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let then_br = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));
        let else_br = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(42))));
        ExprKind::Branch {
            cond,
            then_br,
            else_br,
        }
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[1].opcode, Opcode::BrFalse);
    assert_eq!(instrs[1].operand, Operand::I16(4)); // LdNil(1) + BrJmp(3)
    assert_eq!(instrs[2], Instruction::simple(Opcode::LdNil));
    assert_eq!(instrs[3].opcode, Opcode::BrJmp);
    assert_eq!(instrs[3].operand, Operand::I16(3)); // LdSmi(42)(3)
    assert_eq!(instrs[4], Instruction::with_i16(Opcode::LdSmi, 42));
}

#[test]
fn emit_sequence_pops_intermediates() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let a = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let b = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        ExprKind::Seq(vec![a, b])
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[1], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[2], Instruction::with_i16(Opcode::LdSmi, 2));
}

#[test]
fn emit_record_lit() {
    let typed_module = build_typed_module_single(|ast, interner| {
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
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(
        instrs[0],
        Instruction::with_type_len(Opcode::ArrNew, format::BUILTIN_TYPE_ANY, 1)
    );
    assert_eq!(instrs[1], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[2], Instruction::with_u8(Opcode::ArrSetI, 0));
}

#[test]
fn emit_return_with_value() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let val = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(5))));
        ExprKind::Return(Some(val))
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 5));
    assert_eq!(instrs[1], Instruction::simple(Opcode::Ret));
}

#[test]
fn emit_return_unit() {
    let typed_module = build_typed_module_single(|_ast, _int| ExprKind::Return(None));
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdUnit));
    assert_eq!(instrs[1], Instruction::simple(Opcode::Ret));
}

#[test]
fn emit_empty_module_produces_no_methods() {
    let typed_module = build_typed_module(&[]);
    let module = emit(&typed_module).unwrap();
    assert!(module.methods.is_empty());
}

#[test]
fn emit_negative_smi() {
    let typed_module = build_typed_module_single(|_ast, _int| ExprKind::Lit(Literal::Int(-100)));
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, -100));
}

#[test]
fn emit_array_lit() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let a = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let b = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        ExprKind::ArrayLit(vec![a, b])
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(
        instrs[0],
        Instruction::with_type_len(Opcode::ArrNew, BuiltinType::Array.type_id(), 2)
    );
    assert_eq!(instrs[1], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[2], Instruction::with_u8(Opcode::ArrSetI, 0));
    assert_eq!(instrs[3], Instruction::with_i16(Opcode::LdSmi, 2));
    assert_eq!(instrs[4], Instruction::with_u8(Opcode::ArrSetI, 1));
}

#[test]
fn emit_index_single() {
    let typed_module = build_typed_module_single(|ast, _int| {
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
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdNil));
    assert_eq!(instrs[1], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[2], Instruction::simple(Opcode::ArrGet));
}

#[test]
fn emit_index_chained() {
    let typed_module = build_typed_module_single(|ast, _int| {
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
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdNil));
    assert_eq!(instrs[1], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[2], Instruction::simple(Opcode::ArrGet));
    assert_eq!(instrs[3], Instruction::with_i16(Opcode::LdSmi, 2));
    assert_eq!(instrs[4], Instruction::simple(Opcode::ArrGet));
}

#[test]
fn emit_fstr_single_lit() {
    let typed_module = build_typed_module_single(|_ast, _int| {
        ExprKind::FStrLit(vec![FStrPart::Lit(String::from("hello"))])
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0].opcode, Opcode::LdConst);
    assert_eq!(module.constants.len(), 1);
}

#[test]
fn emit_fstr_multiple_parts() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let expr = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(42))));
        ExprKind::FStrLit(vec![
            FStrPart::Lit(String::from("x is ")),
            FStrPart::Expr(expr),
        ])
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0].opcode, Opcode::LdConst);
    assert_eq!(instrs[1], Instruction::with_i16(Opcode::LdSmi, 42));
    assert_eq!(instrs[2], Instruction::simple(Opcode::ArrCaten));
}

#[test]
fn emit_range_inclusive() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let lhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let rhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(10))));
        ExprKind::BinOp(BinOp::Range, lhs, rhs)
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0].opcode, Opcode::ArrNewT);
    assert!(matches!(instrs[0].operand, Operand::TypeTagged(_, _, 2)));
    assert_eq!(instrs[1], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[2], Instruction::with_u8(Opcode::ArrSetI, 0));
    assert_eq!(instrs[3], Instruction::with_i16(Opcode::LdSmi, 10));
    assert_eq!(instrs[4], Instruction::with_u8(Opcode::ArrSetI, 1));
}

#[test]
fn emit_range_exclusive() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let lhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));
        let rhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(5))));
        ExprKind::BinOp(BinOp::RangeExcl, lhs, rhs)
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0].opcode, Opcode::ArrNewT);
    assert!(matches!(instrs[0].operand, Operand::TypeTagged(_, _, 2)));
    assert_eq!(instrs[1], Instruction::simple(Opcode::LdNil));
    assert_eq!(instrs[2], Instruction::with_u8(Opcode::ArrSetI, 0));
    assert_eq!(instrs[3], Instruction::with_i16(Opcode::LdSmi, 5));
    assert_eq!(instrs[4], Instruction::with_u8(Opcode::ArrSetI, 1));
}

#[test]
fn emit_perform() {
    let mut typed_module = build_typed_module_single(|ast, _int| {
        let operand = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(7))));
        ExprKind::Perform(operand)
    });
    let effect_name = typed_module.db.interner.intern("Test");
    let effect_id = typed_module.type_env.assign_effect_id(effect_name);
    let unit_ty = typed_module.type_env.intern(Ty::Unit);
    let _ = typed_module.type_env.register_effect_ops(
        effect_name,
        vec![(typed_module.db.interner.intern("emit"), None, unit_ty)],
    );
    let perform_expr = typed_module.db.ast.root[0];
    let _ = typed_module.type_env.perform_effects.insert(
        perform_expr,
        EffectUse {
            effect_id,
            op_id: 0,
        },
    );
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 7));
    assert_eq!(instrs[1], Instruction::with_effect(Opcode::EffInvk, 0, 0));
}

#[test]
fn emit_handle_with_body() {
    let mut typed_module = build_typed_module_single(|ast, interner| {
        let body_expr = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let handler_body = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        let eff_sym = interner.intern("MyEff");
        ExprKind::Handle(Box::new(HandleData {
            effect: TyRef {
                name: Ident::dummy(eff_sym),
                args: Vec::new(),
            },
            body: body_expr,
            clauses: vec![
                HandlerClause::Op {
                    name: Ident::dummy(eff_sym),
                    args: vec![],
                    cont: Ident::dummy(interner.intern("k")),
                    body: handler_body,
                },
                HandlerClause::Return {
                    binder: Ident::dummy(interner.intern("x")),
                    body: body_expr,
                },
            ],
        }))
    });
    let eff_sym = typed_module.db.interner.intern("MyEff");
    let ret_ty = typed_module.type_env.intern(Ty::Unit);
    let effect_id = typed_module.type_env.assign_effect_id(eff_sym);
    let _ = typed_module
        .type_env
        .register_effect_ops(eff_sym, vec![(eff_sym, None, ret_ty)]);
    let handle_expr = typed_module.db.ast.root[0];
    let _ = typed_module
        .type_env
        .handle_effects
        .insert(handle_expr, effect_id);
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(
        instrs[0],
        Instruction::with_effect_jump(Opcode::EffHdlPush, 0, 0, 6)
    );
    assert_eq!(instrs[1], Instruction::with_u8(Opcode::StLoc, 0));
    assert_eq!(instrs[2], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[3], Instruction::with_i16(Opcode::LdSmi, 2));
    assert_eq!(instrs[4], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[5], Instruction::with_u8(Opcode::StLoc, 1));
    assert_eq!(instrs[6], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[7], Instruction::simple(Opcode::EffHdlPop));
}

#[test]
fn emit_resume_with_value() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let val = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(3))));
        ExprKind::Resume(Some(val))
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 3));
    assert_eq!(instrs[1], Instruction::with_u8(Opcode::EffCont, 1));
}

#[test]
fn emit_resume_unit() {
    let typed_module = build_typed_module_single(|_ast, _int| ExprKind::Resume(None));
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_u8(Opcode::EffCont, 0));
}

#[test]
fn emit_match_literal_pattern() {
    let typed_module = build_typed_module_single(|ast, _int| {
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
        ExprKind::Case(Box::new(CaseData {
            scrutinee,
            arms: vec![
                CaseArm {
                    attrs: Vec::new(),
                    pat: pat_lit,
                    guard: None,
                    body: body1,
                },
                CaseArm {
                    attrs: Vec::new(),
                    pat: pat_wild,
                    guard: None,
                    body: body2,
                },
            ],
        }))
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
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
    let typed_module = build_typed_module_single(|ast, interner| {
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
        ExprKind::Case(Box::new(CaseData {
            scrutinee,
            arms: vec![CaseArm {
                attrs: Vec::new(),
                pat: tuple_pat,
                guard: None,
                body: var_a,
            }],
        }))
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdNil));
    assert_eq!(instrs[1], Instruction::simple(Opcode::Dup));
    assert_eq!(instrs[2], Instruction::with_u8(Opcode::ArrGetI, 0));
    assert_eq!(instrs[3], Instruction::with_u8(Opcode::StLoc, 0));
    assert_eq!(instrs[4], Instruction::simple(Opcode::Dup));
    assert_eq!(instrs[5], Instruction::with_u8(Opcode::ArrGetI, 1));
    assert_eq!(instrs[6], Instruction::with_u8(Opcode::StLoc, 1));
    assert_eq!(instrs[7], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[8], Instruction::with_u8(Opcode::LdLoc, 0));
}

#[test]
fn emit_match_with_guard() {
    let typed_module = build_typed_module_single(|ast, interner| {
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
        ExprKind::Case(Box::new(CaseData {
            scrutinee,
            arms: vec![
                CaseArm {
                    attrs: Vec::new(),
                    pat: pat_bind,
                    guard: Some(guard_expr),
                    body: body1,
                },
                CaseArm {
                    attrs: Vec::new(),
                    pat: pat_wild,
                    guard: None,
                    body: body2,
                },
            ],
        }))
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
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
    let mut interner = Interner::new();
    let mut ast = AstData::new();

    let x_sym = interner.intern("x");

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

    let var_x = ast
        .exprs
        .alloc(Spanned::dummy(ExprKind::Var(Ident::dummy(x_sym))));

    let lambda_expr = ast.exprs.alloc(Spanned::dummy(ExprKind::Lambda {
        params: Vec::new(),
        ret_ty: None,
        body: var_x,
    }));

    let seq = ast
        .exprs
        .alloc(Spanned::dummy(ExprKind::Seq(vec![let_x, lambda_expr])));
    ast.root.push(seq);

    let db = Db::new(ast, interner, SourceMap::default());
    let mut resolution = ResolutionMap::new();
    let _ = resolution.captures.insert(lambda_expr, vec![x_sym]);
    let type_env = TypeEnv::new();
    let typed_module = TypedModule::new(db, resolution, type_env);

    let module = emit(&typed_module).unwrap();

    let main = &module.methods.last().unwrap();
    let instrs = &main.instructions;

    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 42));
    assert_eq!(instrs[1], Instruction::with_u8(Opcode::StLoc, 0));
    assert_eq!(instrs[2], Instruction::simple(Opcode::LdUnit));
    assert_eq!(instrs[3], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[4], Instruction::with_u8(Opcode::LdLoc, 0));
    assert_eq!(instrs[5], Instruction::with_wide(Opcode::ClsNew, 0, 1));

    let lambda_method = &module.methods[0];
    let lambda_instrs = &lambda_method.instructions;
    assert_eq!(lambda_instrs[0], Instruction::with_u16(Opcode::LdUpv, 0));
    assert_eq!(lambda_instrs[1], Instruction::simple(Opcode::Ret));
}

#[test]
fn emit_matrix_lit() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let a = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let b = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        let c = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(3))));
        let d = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(4))));
        ExprKind::MatrixLit(vec![vec![a, b], vec![c, d]])
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(
        instrs[0],
        Instruction::with_type_len(Opcode::ArrNew, BuiltinType::Array.type_id(), 2)
    );
    assert_eq!(
        instrs[1],
        Instruction::with_type_len(Opcode::ArrNew, BuiltinType::Array.type_id(), 2)
    );
    assert_eq!(instrs[2], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[3], Instruction::with_u8(Opcode::ArrSetI, 0));
    assert_eq!(instrs[4], Instruction::with_i16(Opcode::LdSmi, 2));
    assert_eq!(instrs[5], Instruction::with_u8(Opcode::ArrSetI, 1));
    assert_eq!(instrs[6], Instruction::with_u8(Opcode::ArrSetI, 0));
    assert_eq!(
        instrs[7],
        Instruction::with_type_len(Opcode::ArrNew, BuiltinType::Array.type_id(), 2)
    );
    assert_eq!(instrs[8], Instruction::with_i16(Opcode::LdSmi, 3));
    assert_eq!(instrs[9], Instruction::with_u8(Opcode::ArrSetI, 0));
    assert_eq!(instrs[10], Instruction::with_i16(Opcode::LdSmi, 4));
    assert_eq!(instrs[11], Instruction::with_u8(Opcode::ArrSetI, 1));
    assert_eq!(instrs[12], Instruction::with_u8(Opcode::ArrSetI, 1));
}

#[test]
fn emit_record_update() {
    let typed_module = build_typed_module_single(|ast, interner| {
        let base = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));
        let x_sym = interner.intern("x");
        let val = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(42))));
        let field = RecordField::Named {
            name: Ident::new(x_sym, Span::DUMMY),
            value: Some(val),
        };
        ExprKind::RecordUpdate {
            base,
            fields: vec![field],
        }
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdNil));
    assert_eq!(instrs[1], Instruction::simple(Opcode::ArrCopy));
    assert_eq!(instrs[2], Instruction::with_i16(Opcode::LdSmi, 42));
    assert_eq!(instrs[3], Instruction::with_u8(Opcode::ArrSetI, 0));
}

#[test]
fn emit_postfix_force() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let inner = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(5))));
        ExprKind::Postfix {
            expr: inner,
            op: PostfixOp::Force,
        }
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 5));
    assert_eq!(instrs[1], Instruction::simple(Opcode::Dup));
    assert_eq!(instrs[2], Instruction::simple(Opcode::ArrTag));
    assert_eq!(instrs[3].opcode, Opcode::LdConst);
    assert_eq!(instrs[4], Instruction::simple(Opcode::CmpEq));
    assert_eq!(instrs[5].opcode, Opcode::BrFalse);
    assert_eq!(instrs[6], Instruction::simple(Opcode::Panic));
    assert_eq!(instrs[7], Instruction::with_u8(Opcode::ArrGetI, 0));
}

#[test]
fn emit_postfix_propagate() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let inner = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(5))));
        ExprKind::Postfix {
            expr: inner,
            op: PostfixOp::Propagate,
        }
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 5));
    assert_eq!(instrs[1], Instruction::simple(Opcode::Dup));
    assert_eq!(instrs[2], Instruction::simple(Opcode::ArrTag));
    assert_eq!(instrs[3].opcode, Opcode::LdConst);
    assert_eq!(instrs[4], Instruction::simple(Opcode::CmpEq));
    assert_eq!(instrs[5].opcode, Opcode::BrFalse);
    assert_eq!(instrs[6], Instruction::simple(Opcode::Ret));
    assert_eq!(instrs[7], Instruction::with_u8(Opcode::ArrGetI, 0));
}

#[test]
fn emit_type_op_cast() {
    let typed_module = build_typed_module_single(|ast, interner| {
        let inner = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let int_sym = interner.intern("Int");
        let ty_id = ast.types.alloc(Spanned::dummy(TyKind::Named {
            name: Ident::dummy(int_sym),
            args: vec![],
        }));
        ExprKind::TypeOp {
            expr: inner,
            ty: ty_id,
            kind: TypeOpKind::Cast,
        }
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdOne));
    assert_eq!(
        instrs[1],
        Instruction::with_u16(Opcode::TyCast, BUILTIN_TYPE_INT)
    );
}

#[test]
fn emit_type_op_test() {
    let typed_module = build_typed_module_single(|ast, interner| {
        let inner = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let int_sym = interner.intern("Int");
        let ty_id = ast.types.alloc(Spanned::dummy(TyKind::Named {
            name: Ident::dummy(int_sym),
            args: vec![],
        }));
        ExprKind::TypeOp {
            expr: inner,
            ty: ty_id,
            kind: TypeOpKind::Test(None),
        }
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdOne));
    assert_eq!(
        instrs[1],
        Instruction::with_u16(Opcode::TyChk, BUILTIN_TYPE_INT)
    );
}

#[test]
fn emit_nil_coalesce() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let lhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let rhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        ExprKind::BinOp(BinOp::NilCoalesce, lhs, rhs)
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[1], Instruction::simple(Opcode::Dup));
    assert_eq!(instrs[2], Instruction::simple(Opcode::ArrTag));
    assert_eq!(instrs[3].opcode, Opcode::LdConst);
    assert_eq!(instrs[4], Instruction::simple(Opcode::CmpEq));
    assert_eq!(instrs[5].opcode, Opcode::BrFalse);
    assert_eq!(instrs[6], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[7], Instruction::with_i16(Opcode::LdSmi, 2));
}

#[test]
fn emit_instance_def_via() {
    let typed_module = build_typed_module_single(|_ast, interner| {
        let cls_sym = interner.intern("Show");
        ExprKind::InstanceDef(Box::new(InstanceDef {
            attrs: Vec::new(),
            exported: false,
            ty_params: Vec::new(),
            constraints: Vec::new(),
            ty: TyRef {
                name: Ident::dummy(cls_sym),
                args: Vec::new(),
            },
            body: InstanceBody::Via(TyRef {
                name: Ident::dummy(cls_sym),
                args: Vec::new(),
            }),
        }))
    });
    let module = emit(&typed_module).unwrap();
    // Via instances produce no bytecode at the definition site
    assert!(module.methods.is_empty());
}

#[test]
fn emit_instance_def_methods() {
    let typed_module = build_typed_module_single(|ast, interner| {
        let cls_sym = interner.intern("Show");
        let show_sym = interner.intern("show");
        let body = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let self_sym = interner.intern("self");
        let param = Param {
            mutable: false,
            name: Ident::dummy(self_sym),
            ty: None,
            default: None,
        };
        let decl = FnDecl {
            attrs: vec![],
            name: MemberName::Ident(Ident::dummy(show_sym)),
            params: Some(vec![param]),
            ret_ty: None,
            body: Some(body),
        };
        ExprKind::InstanceDef(Box::new(InstanceDef {
            attrs: Vec::new(),
            exported: false,
            ty_params: Vec::new(),
            constraints: Vec::new(),
            ty: TyRef {
                name: Ident::dummy(cls_sym),
                args: Vec::new(),
            },
            body: InstanceBody::Methods(vec![MemberDecl::Fn(decl)]),
        }))
    });
    let module = emit(&typed_module).unwrap();
    assert_eq!(module.methods.len(), 1);
    let method = &module.methods[0];
    assert!(method.name.is_some());
    let method_instrs = &method.instructions;
    assert_eq!(method_instrs[0], Instruction::simple(Opcode::LdOne));
    assert_eq!(method_instrs[1], Instruction::simple(Opcode::Ret));
}

#[test]
fn emit_foreign_import() {
    let typed_module = build_typed_module_single(|_ast, interner| {
        let sym = interner.intern("libc.so");
        ExprKind::ForeignImport(sym)
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_u16(Opcode::FfiCall, 0));
    assert_eq!(module.foreigns.len(), 1);
}

#[test]
fn emit_comprehension_single_generator() {
    let typed_module = build_typed_module_single(|ast, interner| {
        let x_sym = interner.intern("x");

        let body = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Var(Ident::dummy(x_sym))));

        let e1 = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let e2 = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        let e3 = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(3))));
        let iter_arr = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::ArrayLit(vec![e1, e2, e3])));

        let pat = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(x_sym))));

        ExprKind::Comprehension(Box::new(ComprehensionData {
            expr: body,
            clauses: vec![CompClause::Generator {
                pat,
                iter: iter_arr,
            }],
        }))
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;

    assert_eq!(
        instrs[0],
        Instruction::with_type_len(Opcode::ArrNew, BuiltinType::Array.type_id(), 0)
    );

    assert_eq!(
        instrs[1],
        Instruction::with_type_len(Opcode::ArrNew, BuiltinType::Array.type_id(), 3)
    );

    let iter_store = 8;
    assert_eq!(instrs[iter_store].opcode, Opcode::StLoc);

    assert_eq!(instrs[iter_store + 1], Instruction::simple(Opcode::LdNil));
    assert_eq!(instrs[iter_store + 2].opcode, Opcode::StLoc);

    let lp = iter_store + 3;
    assert_eq!(instrs[lp].opcode, Opcode::LdLoc);
    assert_eq!(instrs[lp + 1].opcode, Opcode::LdLoc);
    assert_eq!(instrs[lp + 2], Instruction::simple(Opcode::ArrLen));
    assert_eq!(instrs[lp + 3], Instruction::simple(Opcode::CmpGeq));
    assert_eq!(instrs[lp + 4].opcode, Opcode::BrTrue);

    assert_eq!(instrs[lp + 5].opcode, Opcode::LdLoc);
    assert_eq!(instrs[lp + 6].opcode, Opcode::LdLoc);
    assert_eq!(instrs[lp + 7], Instruction::simple(Opcode::ArrGet));

    assert_eq!(instrs[lp + 8].opcode, Opcode::StLoc);

    assert_eq!(instrs[lp + 9].opcode, Opcode::LdLoc);
    assert_eq!(instrs[lp + 10], Instruction::simple(Opcode::ArrCaten));

    assert_eq!(instrs[lp + 11].opcode, Opcode::LdLoc);
    assert_eq!(instrs[lp + 12], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[lp + 13], Instruction::simple(Opcode::IAdd));
    assert_eq!(instrs[lp + 14].opcode, Opcode::StLoc);

    assert_eq!(instrs[lp + 15].opcode, Opcode::BrBack);
    assert!(
        matches!(instrs[lp + 15].operand, Operand::I16(off) if off < 0),
        "backward jump should have negative offset"
    );
}

#[test]
fn emit_comprehension_with_filter() {
    let typed_module = build_typed_module_single(|ast, interner| {
        let x_sym = interner.intern("x");

        let body = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Var(Ident::dummy(x_sym))));

        let e1 = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let iter_arr = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::ArrayLit(vec![e1])));

        let pat = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(x_sym))));

        let guard = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));

        ExprKind::Comprehension(Box::new(ComprehensionData {
            expr: body,
            clauses: vec![
                CompClause::Generator {
                    pat,
                    iter: iter_arr,
                },
                CompClause::Filter(guard),
            ],
        }))
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;

    assert_eq!(
        instrs[0],
        Instruction::with_type_len(Opcode::ArrNew, BuiltinType::Array.type_id(), 0)
    );

    let has_filter_branch = instrs.iter().any(|i| i.opcode == Opcode::BrFalse);
    assert!(
        has_filter_branch,
        "comprehension with filter should emit BrFalse"
    );

    let has_concat = instrs.iter().any(|i| i.opcode == Opcode::ArrCaten);
    assert!(has_concat, "comprehension should emit ArrCaten");

    let has_backward_jump = instrs
        .iter()
        .any(|i| i.opcode == Opcode::BrBack && matches!(i.operand, Operand::I16(off) if off < 0));
    assert!(
        has_backward_jump,
        "comprehension should have backward BrBack"
    );
}

#[test]
fn emit_match_or_pattern_literals() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let scrutinee = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(5))));

        let pat1 = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Lit(Literal::Int(1))));
        let pat2 = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Lit(Literal::Int(2))));
        let pat3 = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Lit(Literal::Int(3))));
        let or_pat = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Or(vec![pat1, pat2, pat3])));

        let body1 = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(10))));

        let pat_wild = ast.pats.alloc(Spanned::dummy(PatKind::Wildcard));
        let body2 = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(20))));

        ExprKind::Case(Box::new(CaseData {
            scrutinee,
            arms: vec![
                CaseArm {
                    attrs: Vec::new(),
                    pat: or_pat,
                    guard: None,
                    body: body1,
                },
                CaseArm {
                    attrs: Vec::new(),
                    pat: pat_wild,
                    guard: None,
                    body: body2,
                },
            ],
        }))
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;

    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 5));

    assert_eq!(instrs[1], Instruction::simple(Opcode::Dup));
    assert_eq!(instrs[2], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[3], Instruction::simple(Opcode::CmpEq));
    assert_eq!(instrs[4].opcode, Opcode::BrTrue);

    assert_eq!(instrs[5], Instruction::simple(Opcode::Dup));
    assert_eq!(instrs[6], Instruction::with_i16(Opcode::LdSmi, 2));
    assert_eq!(instrs[7], Instruction::simple(Opcode::CmpEq));
    assert_eq!(instrs[8].opcode, Opcode::BrTrue);

    assert_eq!(instrs[9], Instruction::simple(Opcode::Dup));
    assert_eq!(instrs[10], Instruction::with_i16(Opcode::LdSmi, 3));
    assert_eq!(instrs[11], Instruction::simple(Opcode::CmpEq));
    assert_eq!(instrs[12].opcode, Opcode::BrFalse);

    assert_eq!(instrs[13], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[14], Instruction::with_i16(Opcode::LdSmi, 10));
    assert_eq!(instrs[15].opcode, Opcode::BrJmp);

    assert_eq!(instrs[16], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[17], Instruction::with_i16(Opcode::LdSmi, 20));
}

#[test]
fn emit_match_as_pattern() {
    let typed_module = build_typed_module_single(|ast, interner| {
        let val_sym = interner.intern("val");
        let inner_sym = interner.intern("inner");
        let some_sym = interner.intern("Some");

        let scrutinee = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(5))));

        let inner_bind = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(inner_sym))));
        let variant_pat = ast.pats.alloc(Spanned::dummy(PatKind::Variant {
            tag: Ident::dummy(some_sym),
            fields: vec![inner_bind],
        }));

        let as_pat = ast.pats.alloc(Spanned::dummy(PatKind::As {
            name: Ident::dummy(val_sym),
            pat: variant_pat,
        }));

        let body1 = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Var(Ident::dummy(val_sym))));

        let pat_wild = ast.pats.alloc(Spanned::dummy(PatKind::Wildcard));
        let body2 = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));

        ExprKind::Case(Box::new(CaseData {
            scrutinee,
            arms: vec![
                CaseArm {
                    attrs: Vec::new(),
                    pat: as_pat,
                    guard: None,
                    body: body1,
                },
                CaseArm {
                    attrs: Vec::new(),
                    pat: pat_wild,
                    guard: None,
                    body: body2,
                },
            ],
        }))
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;

    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 5));

    assert_eq!(instrs[1], Instruction::simple(Opcode::Dup));
    assert_eq!(instrs[2], Instruction::with_u8(Opcode::StLoc, 0));

    assert_eq!(instrs[3], Instruction::simple(Opcode::Dup));
    assert_eq!(instrs[4], Instruction::simple(Opcode::ArrTag));
    assert_eq!(instrs[5].opcode, Opcode::LdConst);
    assert_eq!(instrs[6], Instruction::simple(Opcode::CmpEq));
    assert_eq!(instrs[7].opcode, Opcode::BrFalse);

    assert_eq!(instrs[8], Instruction::simple(Opcode::Dup));
    assert_eq!(instrs[9], Instruction::with_u8(Opcode::ArrGetI, 0));
    assert_eq!(instrs[10], Instruction::with_u8(Opcode::StLoc, 1));

    assert_eq!(instrs[11], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[12], Instruction::with_u8(Opcode::LdLoc, 0));
    assert_eq!(instrs[13].opcode, Opcode::BrJmp);

    assert_eq!(instrs[14], Instruction::simple(Opcode::Pop));
    assert_eq!(instrs[15], Instruction::simple(Opcode::LdNil));
}

#[test]
fn emit_wide_locals_above_255() {
    let typed_module = build_typed_module_single(|ast, interner| {
        let mut stmts = Vec::new();
        let syms: Vec<_> = (0u32..257)
            .map(|i| interner.intern(&format!("v{i}")))
            .collect();
        let val = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));
        for &sym in &syms {
            let pat_id = ast
                .pats
                .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(sym))));
            let let_expr = ast
                .exprs
                .alloc(Spanned::dummy(ExprKind::Let(Box::new(LetBinding {
                    modifiers: ModifierSet::default(),
                    attrs: Vec::new(),
                    pat: pat_id,
                    sig: None,
                    value: Some(val),
                }))));
            stmts.push(let_expr);
        }
        let last_sym = *syms.last().unwrap();
        let var_last = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Var(Ident::dummy(last_sym))));
        stmts.push(var_last);
        ExprKind::Seq(stmts)
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;

    let st_loc_w = instrs
        .iter()
        .find(|instr| instr.opcode == Opcode::StLocW && instr.operand == Operand::U16(256));
    assert!(
        st_loc_w.is_some(),
        "expected StLocW for slot 256 in {instrs:?}"
    );

    let ld_loc_w = instrs
        .iter()
        .find(|instr| instr.opcode == Opcode::LdLocW && instr.operand == Operand::U16(256));
    assert!(
        ld_loc_w.is_some(),
        "expected LdLocW for slot 256 in {instrs:?}"
    );

    assert_eq!(instrs[1].opcode, Opcode::StLoc);
    assert_eq!(instrs[1].operand, Operand::U8(0));
}

#[test]
fn emit_index_assign_uses_arr_set() {
    let typed_module = build_typed_module_single(|ast, interner| {
        let arr_sym = interner.intern("arr");
        let i_sym = interner.intern("i");
        let val_sym = interner.intern("v");

        let zero = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));
        let arr_lit = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::ArrayLit(vec![zero])));
        let arr_pat = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(arr_sym))));
        let let_arr = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Let(Box::new(LetBinding {
                modifiers: ModifierSet::default(),
                attrs: Vec::new(),
                pat: arr_pat,
                sig: None,
                value: Some(arr_lit),
            }))));

        let i_val = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));
        let i_pat = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(i_sym))));
        let let_i = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Let(Box::new(LetBinding {
                modifiers: ModifierSet::default(),
                attrs: Vec::new(),
                pat: i_pat,
                sig: None,
                value: Some(i_val),
            }))));

        let v_val = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(99))));
        let v_pat = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(val_sym))));
        let let_v = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Let(Box::new(LetBinding {
                modifiers: ModifierSet::default(),
                attrs: Vec::new(),
                pat: v_pat,
                sig: None,
                value: Some(v_val),
            }))));

        let arr_var = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Var(Ident::dummy(arr_sym))));
        let idx_ref = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Var(Ident::dummy(i_sym))));
        let val_ref = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Var(Ident::dummy(val_sym))));
        let index_expr = ast.exprs.alloc(Spanned::dummy(ExprKind::Index {
            expr: arr_var,
            indices: vec![idx_ref],
            kind: IndexKind::Point,
        }));
        let assign_expr = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Assign(index_expr, val_ref)));

        ExprKind::Seq(vec![let_arr, let_i, let_v, assign_expr])
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    let tail = &instrs[instrs.len() - 5..];

    assert_eq!(tail[0], Instruction::with_u8(Opcode::LdLoc, 0));
    assert_eq!(tail[1], Instruction::with_u8(Opcode::LdLoc, 1));
    assert_eq!(tail[2], Instruction::with_u8(Opcode::LdLoc, 2));
    assert_eq!(tail[3], Instruction::simple(Opcode::ArrSet));
    assert_eq!(tail[4], Instruction::simple(Opcode::Halt));
}

#[test]
fn emit_true_variant_emits_ld_true() {
    let mut typed_module = build_typed_module_single(|_ast, int| {
        let true_sym = int.intern("True");
        ExprKind::VariantLit(Ident::dummy(true_sym), vec![])
    });
    let variant_id = typed_module.db.ast.exprs.iter().next().unwrap().0;
    let _ = typed_module.type_env.dispatch.insert(
        variant_id,
        DispatchInfo::Static {
            opcode: Opcode::LdTru,
        },
    );
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdTru));
}

#[test]
fn emit_false_variant_emits_ld_false() {
    let mut typed_module = build_typed_module_single(|_ast, int| {
        let false_sym = int.intern("False");
        ExprKind::VariantLit(Ident::dummy(false_sym), vec![])
    });
    let variant_id = typed_module.db.ast.exprs.iter().next().unwrap().0;
    let _ = typed_module.type_env.dispatch.insert(
        variant_id,
        DispatchInfo::Static {
            opcode: Opcode::LdFls,
        },
    );
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdFls));
}

#[test]
fn emit_intrinsic_call_emits_opcode() {
    let mut typed_module = build_typed_module_single(|ast, _| {
        let lhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let rhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        ExprKind::BinOp(BinOp::Shl, lhs, rhs)
    });
    let callee_id = typed_module.db.ast.root[0];
    let _ = typed_module.type_env.dispatch.insert(
        callee_id,
        DispatchInfo::Static {
            opcode: Opcode::Shl,
        },
    );
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    let has_ishl = instrs.iter().any(|i| i.opcode == Opcode::Shl);
    assert!(has_ishl, "expected Shl in {instrs:?}");
}

// --- Phase 2.5A: Float dispatch tests ---

#[test]
fn emit_float_binop_uses_dispatch() {
    let mut typed_module = build_typed_module_single(|ast, _int| {
        let lhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Float(1.0))));
        let rhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Float(2.0))));
        ExprKind::BinOp(BinOp::Add, lhs, rhs)
    });
    let binop_id = typed_module.db.ast.root[0];
    let _ = typed_module.type_env.dispatch.insert(
        binop_id,
        DispatchInfo::Static {
            opcode: Opcode::FAdd,
        },
    );
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    let has_fadd = instrs.iter().any(|i| i.opcode == Opcode::FAdd);
    assert!(has_fadd, "expected FAdd in {instrs:?}");
    let has_iadd = instrs.iter().any(|i| i.opcode == Opcode::IAdd);
    assert!(
        !has_iadd,
        "should not have IAdd when dispatch provides f.add"
    );
}

#[test]
fn emit_float_neg_uses_dispatch() {
    let mut typed_module = build_typed_module_single(|ast, _int| {
        let operand = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Float(3.0))));
        ExprKind::UnaryOp(UnaryOp::Neg, operand)
    });
    let neg_id = typed_module.db.ast.root[0];
    let _ = typed_module.type_env.dispatch.insert(
        neg_id,
        DispatchInfo::Static {
            opcode: Opcode::FNeg,
        },
    );
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    let has_fneg = instrs.iter().any(|i| i.opcode == Opcode::FNeg);
    assert!(has_fneg, "expected FNeg in {instrs:?}");
}

// --- Phase 2.5B: Test coverage gaps ---

#[test]
fn emit_var_global() {
    let typed_module = build_typed_module(&[
        |ast, interner| {
            let x_sym = interner.intern("x");
            let pat_id = ast
                .pats
                .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(x_sym))));
            let value = ast
                .exprs
                .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(99))));
            ExprKind::Let(Box::new(LetBinding {
                modifiers: ModifierSet::default(),
                attrs: Vec::new(),
                pat: pat_id,
                sig: None,
                value: Some(value),
            }))
        },
        |_ast, interner| {
            let x_sym = interner.intern("x");
            ExprKind::Var(Ident::dummy(x_sym))
        },
    ]);
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    let has_ldglb = instrs.iter().any(|i| i.opcode == Opcode::LdGlob);
    assert!(has_ldglb, "expected LdGlob in {instrs:?}");
}

#[test]
fn emit_access_positional() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let record = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));
        ExprKind::Access {
            expr: record,
            field: FieldTarget::Index(2),
            mode: AccessMode::Direct,
        }
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[1], Instruction::with_u8(Opcode::ArrGetI, 2));
}

#[test]
fn emit_unary_neg() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let operand = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(5))));
        ExprKind::UnaryOp(UnaryOp::Neg, operand)
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::with_i16(Opcode::LdSmi, 5));
    assert_eq!(instrs[1], Instruction::simple(Opcode::INeg));
}

#[test]
fn emit_unary_not() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let operand = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        ExprKind::UnaryOp(UnaryOp::Not, operand)
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(instrs[0], Instruction::simple(Opcode::LdOne));
    assert_eq!(instrs[1], Instruction::simple(Opcode::Not));
}

#[test]
fn emit_tuple_lit() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let a = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let b = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        let c = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(3))));
        ExprKind::TupleLit(vec![a, b, c])
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    assert_eq!(
        instrs[0],
        Instruction::with_type_len(Opcode::ArrNew, format::BUILTIN_TYPE_ANY, 3)
    );
    assert_eq!(instrs[2], Instruction::with_u8(Opcode::ArrSetI, 0));
    assert_eq!(instrs[4], Instruction::with_u8(Opcode::ArrSetI, 1));
    assert_eq!(instrs[6], Instruction::with_u8(Opcode::ArrSetI, 2));
}

#[test]
fn emit_assign_var() {
    let typed_module = build_typed_module_single(|ast, interner| {
        let x_sym = interner.intern("x");
        let pat_id = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(x_sym))));
        let init = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let let_expr = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Let(Box::new(LetBinding {
                modifiers: ModifierSet::default(),
                attrs: Vec::new(),
                pat: pat_id,
                sig: None,
                value: Some(init),
            }))));
        let target = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Var(Ident::dummy(x_sym))));
        let new_val = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        let assign = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Assign(target, new_val)));
        ExprKind::Seq(vec![let_expr, assign])
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    let st_count = instrs.iter().filter(|i| i.opcode == Opcode::StLoc).count();
    assert!(st_count >= 2, "expected at least 2 StLoc in {instrs:?}");
}

#[test]
fn emit_assign_field() {
    let typed_module = build_typed_module_single(|ast, _int| {
        let record = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));
        let target = ast.exprs.alloc(Spanned::dummy(ExprKind::Access {
            expr: record,
            field: FieldTarget::Index(1),
            mode: AccessMode::Direct,
        }));
        let value = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(42))));
        ExprKind::Assign(target, value)
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    let has_arrseti = instrs
        .iter()
        .any(|i| i.opcode == Opcode::ArrSetI && i.operand == Operand::U8(1));
    assert!(has_arrseti, "expected ArrSeti(1) in {instrs:?}");
}

#[test]
fn emit_dictionary_dispatch() {
    let mut typed_module = build_typed_module_single(|ast, _interner| {
        let lhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let rhs = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
        ExprKind::BinOp(BinOp::Add, lhs, rhs)
    });
    let binop_id = typed_module.db.ast.root[0];
    let cls_sym = typed_module.db.interner.intern("Num");
    let _ = typed_module.type_env.dispatch.insert(
        binop_id,
        DispatchInfo::Dictionary {
            class: cls_sym,
            method_idx: 3,
        },
    );
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    let has_tycldict = instrs.iter().any(|i| i.opcode == Opcode::TyclDict);
    let has_tyclcall = instrs
        .iter()
        .any(|i| i.opcode == Opcode::TyclCall && i.operand == Operand::U8(3));
    assert!(has_tycldict, "expected TyclDict in {instrs:?}");
    assert!(has_tyclcall, "expected TyclCall(3) in {instrs:?}");
}

#[test]
fn emit_match_record_pattern() {
    let typed_module = build_typed_module_single(|ast, interner| {
        let scrutinee = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));
        let x_sym = interner.intern("x");
        let y_sym = interner.intern("y");
        let x_bind = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(x_sym))));
        let y_bind = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(y_sym))));
        let pat = ast.pats.alloc(Spanned::dummy(PatKind::Record(vec![
            RecordPatField {
                mutable: false,
                name: Ident::dummy(x_sym),
                pat: Some(x_bind),
            },
            RecordPatField {
                mutable: false,
                name: Ident::dummy(y_sym),
                pat: Some(y_bind),
            },
        ])));
        let body = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let arm = CaseArm {
            attrs: Vec::new(),
            pat,
            guard: None,
            body,
        };
        ExprKind::Case(Box::new(CaseData {
            scrutinee,
            arms: vec![arm],
        }))
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    let geti_count = instrs
        .iter()
        .filter(|i| i.opcode == Opcode::ArrGetI)
        .count();
    assert!(geti_count >= 2, "expected 2+ ArrGeti in {instrs:?}");
}

#[test]
fn emit_match_variant_with_fields() {
    let typed_module = build_typed_module_single(|ast, interner| {
        let scrutinee = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));
        let some_sym = interner.intern("Some");
        let val_sym = interner.intern("val");
        let field_bind = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(val_sym))));
        let pat = ast.pats.alloc(Spanned::dummy(PatKind::Variant {
            tag: Ident::dummy(some_sym),
            fields: vec![field_bind],
        }));
        let body = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
        let arm = CaseArm {
            attrs: Vec::new(),
            pat,
            guard: None,
            body,
        };
        ExprKind::Case(Box::new(CaseData {
            scrutinee,
            arms: vec![arm],
        }))
    });
    let module = emit(&typed_module).unwrap();
    let instrs = &module.methods[0].instructions;
    let has_arrtag = instrs.iter().any(|i| i.opcode == Opcode::ArrTag);
    let has_geti = instrs.iter().any(|i| i.opcode == Opcode::ArrGetI);
    assert!(has_arrtag, "expected ArrTag in {instrs:?}");
    assert!(has_geti, "expected ArrGeti for field binding in {instrs:?}");
}

#[test]
fn emit_top_level_function() {
    let typed_module = build_typed_module(&[|ast, interner| {
        let f_sym = interner.intern("f");
        let x_sym = interner.intern("x");
        let pat_id = ast
            .pats
            .alloc(Spanned::dummy(PatKind::Bind(Ident::dummy(f_sym))));
        let body = ast
            .exprs
            .alloc(Spanned::dummy(ExprKind::Var(Ident::dummy(x_sym))));
        let param = Param {
            mutable: false,
            name: Ident::dummy(x_sym),
            ty: None,
            default: None,
        };
        ExprKind::Let(Box::new(LetBinding {
            modifiers: ModifierSet::default(),
            attrs: Vec::new(),
            pat: pat_id,
            sig: Some(Box::new(Signature {
                has_param_list: true,
                params: vec![param],
                ty_params: Vec::new(),
                constraints: Vec::new(),
                effects: Vec::new(),
                ret_ty: None,
            })),
            value: Some(body),
        }))
    }]);
    let module = emit(&typed_module).unwrap();
    assert_eq!(module.methods.len(), 2);

    let fn_method = module
        .methods
        .iter()
        .find(|method| method.name.is_some())
        .unwrap();
    assert_eq!(
        fn_method.instructions[0],
        Instruction::with_u8(Opcode::LdLoc, 0)
    );
    assert_eq!(fn_method.instructions[1], Instruction::simple(Opcode::Ret));

    let main = module
        .methods
        .iter()
        .find(|method| method.name.is_none())
        .unwrap();
    assert!(main.instructions.iter().any(|instr| matches!(
        instr,
        Instruction {
            opcode: Opcode::ClsNew,
            ..
        }
    )));
    assert!(main.instructions.iter().any(|instr| matches!(
        instr,
        Instruction {
            opcode: Opcode::StGlob,
            operand: Operand::U16(0),
        }
    )));
}
