use music_builtins::types::BuiltinType;
use music_found::Interner;

use crate::effects::{check_purity, remove_handled_effect};
use crate::env::TypeEnv;
use crate::types::Ty;

fn seeded_env() -> TypeEnv {
    let mut env = TypeEnv::new();
    env.seed_builtins();
    env
}

#[test]
fn pure_function_no_effects_ok() {
    let mut env = seeded_env();
    let int = env.builtin(BuiltinType::Int);
    let arrow = env.intern(Ty::Arrow {
        param: int,
        ret: int,
    });
    let result = check_purity(&env, arrow, &[]);
    assert!(result.is_none());
}

#[test]
fn pure_function_with_effect_violation() {
    let mut env = seeded_env();
    let mut interner = Interner::new();
    let int = env.builtin(BuiltinType::Int);
    let arrow = env.intern(Ty::Arrow {
        param: int,
        ret: int,
    });
    let console = interner.intern("Console");
    let effect = env.intern(Ty::Effect(console));
    let result = check_purity(&env, arrow, &[effect]);
    assert_eq!(result, Some(effect));
}

#[test]
fn effectful_function_with_matching_effect_ok() {
    let mut env = seeded_env();
    let mut interner = Interner::new();
    let int = env.builtin(BuiltinType::Int);
    let console = interner.intern("Console");
    let effect = env.intern(Ty::Effect(console));
    let eff_arrow = env.intern(Ty::EffectArrow {
        param: int,
        ret: int,
        effects: vec![effect],
    });
    let result = check_purity(&env, eff_arrow, &[effect]);
    assert!(result.is_none());
}

#[test]
fn remove_handled_effect_filters_correctly() {
    let mut env = seeded_env();
    let mut interner = Interner::new();
    let console = interner.intern("Console");
    let state = interner.intern("State");
    let eff_console = env.intern(Ty::Effect(console));
    let eff_state = env.intern(Ty::Effect(state));

    let active = vec![eff_console, eff_state];
    let remaining = remove_handled_effect(&env, &active, eff_console);
    assert_eq!(remaining.len(), 1);
    assert_eq!(remaining[0], eff_state);
}

#[test]
fn remove_handled_effect_no_match_keeps_all() {
    let mut env = seeded_env();
    let mut interner = Interner::new();
    let console = interner.intern("Console");
    let state = interner.intern("State");
    let io = interner.intern("IO");
    let eff_console = env.intern(Ty::Effect(console));
    let eff_state = env.intern(Ty::Effect(state));
    let eff_io = env.intern(Ty::Effect(io));

    let active = vec![eff_console, eff_state];
    let remaining = remove_handled_effect(&env, &active, eff_io);
    assert_eq!(remaining.len(), 2);
}

#[test]
fn non_arrow_type_no_purity_constraint() {
    let env = seeded_env();
    let int = env.builtin(BuiltinType::Int);
    let result = check_purity(&env, int, &[int]);
    assert!(result.is_none());
}
