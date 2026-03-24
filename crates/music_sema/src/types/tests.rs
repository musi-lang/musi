use music_arena::Arena;
use music_builtins::types::BuiltinType;
use music_found::Interner;

use crate::types::{SemaTypeId, Ty};

#[test]
fn sema_type_id_is_idx_of_ty() {
    let mut arena = Arena::<Ty>::new();
    let id: SemaTypeId = arena.alloc(Ty::Unit);
    assert_eq!(*arena.get(id), Ty::Unit);
}

#[test]
fn builtin_ty_equality() {
    assert_eq!(Ty::Builtin(BuiltinType::Int), Ty::Builtin(BuiltinType::Int));
    assert_ne!(
        Ty::Builtin(BuiltinType::Int),
        Ty::Builtin(BuiltinType::Float)
    );
}

#[test]
fn gradual_types_are_distinct() {
    assert_ne!(Ty::Any, Ty::Unknown);
    assert_ne!(Ty::Any, Ty::Never);
    assert_ne!(Ty::Unknown, Ty::Never);
    assert_ne!(Ty::Unit, Ty::Any);
}

#[test]
fn record_with_fields() {
    let mut arena = Arena::<Ty>::new();
    let mut interner = Interner::new();
    let int_id = arena.alloc(Ty::Builtin(BuiltinType::Int));
    let sym = interner.intern("x");

    let record = Ty::Record {
        fields: vec![(sym, int_id)],
    };
    let record_id = arena.alloc(record.clone());
    assert_eq!(*arena.get(record_id), record);
}

#[test]
fn arrow_type_construction() {
    let mut arena = Arena::<Ty>::new();
    let int_id = arena.alloc(Ty::Builtin(BuiltinType::Int));
    let bool_id = arena.alloc(Ty::Builtin(BuiltinType::Bool));

    let arrow = Ty::Arrow {
        param: int_id,
        ret: bool_id,
    };
    let arrow_id = arena.alloc(arrow.clone());
    assert_eq!(*arena.get(arrow_id), arrow);
}

#[test]
fn var_type_stores_id() {
    let var = Ty::Var(42);
    assert_eq!(var, Ty::Var(42));
    assert_ne!(var, Ty::Var(43));
}

#[test]
fn tuple_type_construction() {
    let mut arena = Arena::<Ty>::new();
    let int_id = arena.alloc(Ty::Builtin(BuiltinType::Int));
    let float_id = arena.alloc(Ty::Builtin(BuiltinType::Float));

    let tuple = Ty::Tuple(vec![int_id, float_id]);
    let tuple_id = arena.alloc(tuple.clone());
    assert_eq!(*arena.get(tuple_id), tuple);
}

#[test]
fn choice_with_payload_and_without() {
    let mut arena = Arena::<Ty>::new();
    let mut interner = Interner::new();
    let int_id = arena.alloc(Ty::Builtin(BuiltinType::Int));
    let some_sym = interner.intern("Some");
    let none_sym = interner.intern("None");

    let choice = Ty::Choice {
        variants: vec![(some_sym, Some(int_id)), (none_sym, None)],
    };
    let choice_id = arena.alloc(choice.clone());
    assert_eq!(*arena.get(choice_id), choice);
}

#[test]
fn effect_arrow_with_effects() {
    let mut arena = Arena::<Ty>::new();
    let mut interner = Interner::new();
    let int_id = arena.alloc(Ty::Builtin(BuiltinType::Int));
    let eff_sym = interner.intern("Console");
    let eff_id = arena.alloc(Ty::Effect(eff_sym));

    let eff_arrow = Ty::EffectArrow {
        param: int_id,
        ret: int_id,
        effects: vec![eff_id],
    };
    let id = arena.alloc(eff_arrow.clone());
    assert_eq!(*arena.get(id), eff_arrow);
}

#[test]
fn app_type_construction() {
    let mut arena = Arena::<Ty>::new();
    let mut interner = Interner::new();
    let class_sym = interner.intern("List");
    let list_ty = arena.alloc(Ty::Class(class_sym));
    let int_id = arena.alloc(Ty::Builtin(BuiltinType::Int));

    let app = Ty::App(list_ty, vec![int_id]);
    let app_id = arena.alloc(app.clone());
    assert_eq!(*arena.get(app_id), app);
}
