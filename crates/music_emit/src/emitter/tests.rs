use music_ast::common::ModifierSet;
use music_ast::data::AstData;
use music_ast::expr::{BinOp, ExprKind, LetBinding, RecordField};
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
