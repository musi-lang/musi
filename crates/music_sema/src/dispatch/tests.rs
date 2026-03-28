use music_ast::expr::BinOp;
use music_il::opcode::Opcode;
use music_owned::types::BuiltinType;
use music_shared::Interner;

use crate::dispatch::resolve_binop;
use crate::env::{DispatchInfo, TypeEnv};
use crate::types::Ty;

fn seeded_env() -> TypeEnv {
    let mut env = TypeEnv::new();
    env.seed_builtins();
    env
}

#[test]
fn int_add_resolves_to_static() {
    let mut env = seeded_env();
    let int = env.builtin(BuiltinType::Int);
    let res = resolve_binop(&mut env, BinOp::Add, int, int);
    assert_eq!(res.result_ty, int);
    assert!(matches!(
        res.dispatch,
        Some(DispatchInfo::Static {
            opcode: Opcode::IAdd
        })
    ));
    assert!(res.needs_class.is_none());
}

#[test]
fn int_eq_resolves_to_static_bool() {
    let mut env = seeded_env();
    let int = env.builtin(BuiltinType::Int);
    let bool_ty = env.builtin(BuiltinType::Bool);
    let res = resolve_binop(&mut env, BinOp::Eq, int, int);
    assert_eq!(res.result_ty, bool_ty);
    assert!(matches!(
        res.dispatch,
        Some(DispatchInfo::Static {
            opcode: Opcode::CmpEq
        })
    ));
}

#[test]
fn bool_and_resolves_to_static() {
    let mut env = seeded_env();
    let bool_ty = env.builtin(BuiltinType::Bool);
    let res = resolve_binop(&mut env, BinOp::And, bool_ty, bool_ty);
    assert_eq!(res.result_ty, bool_ty);
    assert!(matches!(
        res.dispatch,
        Some(DispatchInfo::Static {
            opcode: Opcode::And
        })
    ));
}

#[test]
fn int_and_resolves_to_bitwise() {
    let mut env = seeded_env();
    let int = env.builtin(BuiltinType::Int);
    let res = resolve_binop(&mut env, BinOp::And, int, int);
    assert_eq!(res.result_ty, int);
    assert!(matches!(
        res.dispatch,
        Some(DispatchInfo::Static {
            opcode: Opcode::And
        })
    ));
}

#[test]
fn type_param_add_needs_class() {
    let mut env = seeded_env();
    let mut interner = Interner::new();
    let t = interner.intern("T");
    let param_ty = env.intern(Ty::Param(t));
    let res = resolve_binop(&mut env, BinOp::Add, param_ty, param_ty);
    assert_eq!(res.result_ty, param_ty);
    assert!(res.dispatch.is_none());
    assert_eq!(res.needs_class, Some("Num"));
}

#[test]
fn any_add_resolves_to_dynamic() {
    let mut env = seeded_env();
    let any = env.intern(Ty::Any);
    let res = resolve_binop(&mut env, BinOp::Add, any, any);
    assert!(matches!(res.dispatch, Some(DispatchInfo::Dynamic)));
}

#[test]
fn cons_produces_list() {
    let mut env = seeded_env();
    let int = env.builtin(BuiltinType::Int);
    let res = resolve_binop(&mut env, BinOp::Cons, int, int);
    assert!(matches!(env.types.get(res.result_ty), Ty::List(_)));
    assert!(matches!(
        res.dispatch,
        Some(DispatchInfo::Static {
            opcode: Opcode::ArrCaten
        })
    ));
}

#[test]
fn range_produces_tuple() {
    let mut env = seeded_env();
    let int = env.builtin(BuiltinType::Int);
    let res = resolve_binop(&mut env, BinOp::Range, int, int);
    assert!(matches!(env.types.get(res.result_ty), Ty::Tuple(_)));
}

#[test]
fn float_mul_resolves_to_static() {
    let mut env = seeded_env();
    let float = env.builtin(BuiltinType::Float);
    let res = resolve_binop(&mut env, BinOp::Mul, float, float);
    assert_eq!(res.result_ty, float);
    assert!(matches!(
        res.dispatch,
        Some(DispatchInfo::Static {
            opcode: Opcode::FMul
        })
    ));
}

#[test]
fn float_add_resolves_to_static() {
    let mut env = seeded_env();
    let float = env.builtin(BuiltinType::Float);
    let res = resolve_binop(&mut env, BinOp::Add, float, float);
    assert_eq!(res.result_ty, float);
    assert!(matches!(
        res.dispatch,
        Some(DispatchInfo::Static {
            opcode: Opcode::FAdd
        })
    ));
}

#[test]
fn float_sub_resolves_to_static() {
    let mut env = seeded_env();
    let float = env.builtin(BuiltinType::Float);
    let res = resolve_binop(&mut env, BinOp::Sub, float, float);
    assert_eq!(res.result_ty, float);
    assert!(matches!(
        res.dispatch,
        Some(DispatchInfo::Static {
            opcode: Opcode::FSub
        })
    ));
}

#[test]
fn float_div_resolves_to_static() {
    let mut env = seeded_env();
    let float = env.builtin(BuiltinType::Float);
    let res = resolve_binop(&mut env, BinOp::Div, float, float);
    assert_eq!(res.result_ty, float);
    assert!(matches!(
        res.dispatch,
        Some(DispatchInfo::Static {
            opcode: Opcode::FDiv
        })
    ));
}

#[test]
fn int_lt_resolves_to_bool() {
    let mut env = seeded_env();
    let int = env.builtin(BuiltinType::Int);
    let bool_ty = env.builtin(BuiltinType::Bool);
    let res = resolve_binop(&mut env, BinOp::Lt, int, int);
    assert_eq!(res.result_ty, bool_ty);
    assert!(matches!(
        res.dispatch,
        Some(DispatchInfo::Static {
            opcode: Opcode::CmpLt
        })
    ));
}
