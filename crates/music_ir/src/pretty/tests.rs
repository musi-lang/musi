use music_shared::Interner;

use crate::IrModule;
use crate::constant::IrConstValue;
use crate::func::{IrFnId, IrFunction, IrLocal, IrLocalDecl, IrParam, IrParamMode};
use crate::inst::{IrInst, IrOperand, IrRvalue};
use crate::types::{IrEffectMask, IrType};
use music_shared::Span;

fn make_span() -> Span {
    Span::new(0, 0)
}

fn make_simple_module() -> (IrModule, Interner) {
    let mut interner = Interner::new();
    let mut module = IrModule::new();

    let int32_idx = module.types.alloc(IrType::Int32);
    let name = interner.intern("fib");
    let param_local = IrLocal(0);
    let result_local = IrLocal(1);

    let body = vec![
        IrInst::Assign {
            dst: result_local,
            rvalue: IrRvalue::Const(IrConstValue::Int(0)),
            span: make_span(),
        },
        IrInst::Return {
            value: Some(IrOperand::Local(result_local)),
            span: make_span(),
        },
    ];

    let locals = vec![
        IrLocalDecl {
            local: param_local,
            ty: int32_idx,
            mutable: false,
            span: make_span(),
        },
        IrLocalDecl {
            local: result_local,
            ty: int32_idx,
            mutable: false,
            span: make_span(),
        },
    ];

    let params = vec![IrParam {
        local: param_local,
        ty: int32_idx,
        mode: IrParamMode::Value,
        span: make_span(),
    }];

    let fn_idx = module.functions.alloc(IrFunction {
        id: IrFnId(0),
        source_def: None,
        name,
        params,
        ret_ty: int32_idx,
        effects: IrEffectMask::PURE,
        body,
        locals,
        is_closure: false,
        span: make_span(),
    });
    module.entry = Some(fn_idx);
    (module, interner)
}

#[test]
fn test_pretty_module_is_nonempty() {
    let (module, interner) = make_simple_module();
    let output = super::module_to_string(&module, &interner);
    assert!(!output.is_empty(), "pretty output must not be empty");
}

#[test]
fn test_pretty_module_contains_fn_name() {
    let (module, interner) = make_simple_module();
    let output = super::module_to_string(&module, &interner);
    assert!(
        output.contains("@fib"),
        "output must contain function name @fib"
    );
}

#[test]
fn test_pretty_module_contains_return() {
    let (module, interner) = make_simple_module();
    let output = super::module_to_string(&module, &interner);
    assert!(
        output.contains("return"),
        "output must contain return instruction"
    );
}

#[test]
fn test_pretty_module_contains_entry() {
    let (module, interner) = make_simple_module();
    let output = super::module_to_string(&module, &interner);
    assert!(
        output.contains("entry = @fib"),
        "output must declare entry point"
    );
}

#[test]
fn test_pretty_module_braces_balanced() {
    let (module, interner) = make_simple_module();
    let output = super::module_to_string(&module, &interner);
    let opens = output.chars().filter(|&c| c == '{').count();
    let closes = output.chars().filter(|&c| c == '}').count();
    assert_eq!(opens, closes, "braces must be balanced in output");
}

#[test]
fn test_pretty_const_value_appears() {
    let (module, interner) = make_simple_module();
    let output = super::module_to_string(&module, &interner);
    assert!(
        output.contains("const.i 0"),
        "output must contain const.i 0"
    );
}

#[test]
fn test_pretty_fnref_const_displays_correctly() {
    // Arrange: module with a FnRef constant in an assign
    let mut interner = Interner::new();
    let mut module = IrModule::new();

    let unit_idx = module.types.alloc(IrType::Unit);
    let name = interner.intern("has_fnref");
    let dst = IrLocal(0);

    let body = vec![
        IrInst::Assign {
            dst,
            rvalue: IrRvalue::Const(IrConstValue::FnRef(7)),
            span: make_span(),
        },
        IrInst::Return {
            value: None,
            span: make_span(),
        },
    ];
    let locals = vec![IrLocalDecl {
        local: dst,
        ty: unit_idx,
        mutable: false,
        span: make_span(),
    }];
    let fn_idx = module.functions.alloc(IrFunction {
        id: IrFnId(0),
        source_def: None,
        name,
        params: vec![],
        ret_ty: unit_idx,
        effects: IrEffectMask::PURE,
        body,
        locals,
        is_closure: false,
        span: make_span(),
    });
    module.entry = Some(fn_idx);

    // Act
    let output = super::module_to_string(&module, &interner);

    // Assert
    assert!(
        output.contains("const.fn fn#7"),
        "FnRef must render as 'const.fn fn#7'"
    );
}
