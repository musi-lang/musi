//! Pretty-printer for MSIR (debugging and `.msir` text output).
//!
//! Produces a human-readable, line-based text representation of an [`IrModule`].
//! The output format is designed to be readable for debugging and can be used
//! as `.msir` cache files. Write-only (no parse path in Phase 1).
//!
//! # Format example
//!
//! ```text
//! module {
//!   entry = @main
//!
//!   fn @main(%0: Int32) : Int32 {
//!     locals:
//!       %1: Int32
//!       %2: Bool
//!     body:
//!       %1 = const.i 42
//!       return %1
//!   }
//! }
//! ```

#[cfg(test)]
mod tests;

use std::fmt::{self, Write};

use music_shared::{Arena, Idx, Interner};

use crate::IrModule;
use crate::constant::IrConstValue;
use crate::effect::IrEffectDef;
use crate::func::IrFunction;
use crate::inst::{IrBinOp, IrCallee, IrInst, IrOperand, IrPlace, IrRvalue, IrUnaryOp};
use crate::types::{IrEffectMask, IrSumVariant, IrType};

// ── Public entry points ─────────────────────────────────────────────────────

/// Write a complete [`IrModule`] in `.msir` text format.
///
/// # Errors
///
/// Returns [`fmt::Error`] if the underlying writer returns an error.
///
/// # Panics
///
/// Panics if the module contains more than `u32::MAX` functions (impossible on
/// any supported platform since [`Arena`] is also bounded to `u32::MAX`).
pub fn write_module(
    w: &mut impl Write,
    m: &IrModule,
    interner: &Interner,
) -> Result<(), fmt::Error> {
    writeln!(w, "module {{")?;

    // Entry point
    if let Some(entry_idx) = m.entry {
        let entry_fn = &m.functions[entry_idx];
        let name = interner.resolve(entry_fn.name);
        writeln!(w, "  entry = @{name}")?;
        writeln!(w)?;
    }

    // Effect definitions
    for effect in &m.effects {
        write_effect_def(w, effect, interner, &m.types)?;
        writeln!(w)?;
    }

    // Functions
    let fn_count = u32::try_from(m.functions.len()).map_err(|_| fmt::Error)?;
    for raw in 0..fn_count {
        let fn_idx = Idx::from_raw(raw);
        let func = &m.functions[fn_idx];
        write_function(w, func, interner, &m.types)?;
        writeln!(w)?;
    }

    writeln!(w, "}}")?;
    Ok(())
}

/// Format an [`IrModule`] as a [`String`].
///
/// # Panics
///
/// Panics if the module contains more than `u32::MAX` functions (impossible in
/// practice since [`Arena`] is bounded to `u32::MAX` items).
#[must_use]
pub fn module_to_string(m: &IrModule, interner: &Interner) -> String {
    let mut s = String::new();
    write_module(&mut s, m, interner).expect("writing to String never fails");
    s
}

// ── Private helpers ─────────────────────────────────────────────────────────

fn write_effect_def(
    w: &mut impl Write,
    def: &IrEffectDef,
    interner: &Interner,
    types: &Arena<IrType>,
) -> Result<(), fmt::Error> {
    let name = interner.resolve(def.name);
    writeln!(w, "  effect @{name} {{")?;
    for op in &def.ops {
        let op_name = interner.resolve(op.name);
        write!(w, "    op @{op_name}(")?;
        for (i, param_ty) in op.param_tys.iter().enumerate() {
            if i > 0 {
                write!(w, ", ")?;
            }
            write_type(w, *param_ty, types)?;
        }
        write!(w, ") : ")?;
        write_type(w, op.ret_ty, types)?;
        writeln!(w)?;
    }
    writeln!(w, "  }}")
}

fn write_function(
    w: &mut impl Write,
    func: &IrFunction,
    interner: &Interner,
    types: &Arena<IrType>,
) -> Result<(), fmt::Error> {
    let name = interner.resolve(func.name);

    write!(w, "  fn @{name}(")?;
    for (i, param) in func.params.iter().enumerate() {
        if i > 0 {
            write!(w, ", ")?;
        }
        write!(w, "%{}", param.local.0)?;
        write!(w, ": ")?;
        write_type(w, param.ty, types)?;
    }
    write!(w, ") : ")?;
    write_type(w, func.ret_ty, types)?;

    if !func.effects.is_pure() {
        write!(w, " under {}", format_effect_mask(func.effects))?;
    }

    writeln!(w, " {{")?;

    // Locals (skip params — already in signature)
    let param_count = func.params.len();
    let extra_locals: Vec<_> = func
        .locals
        .iter()
        .filter(|d| usize::try_from(d.local.0).expect("local fits usize") >= param_count)
        .collect();

    if !extra_locals.is_empty() {
        writeln!(w, "    locals:")?;
        for decl in &extra_locals {
            write!(w, "      %{}: ", decl.local.0)?;
            write_type(w, decl.ty, types)?;
            writeln!(w)?;
        }
    }

    writeln!(w, "    body:")?;
    for inst in &func.body {
        write_inst(w, inst, interner, types)?;
    }

    writeln!(w, "  }}")
}

fn write_inst(
    w: &mut impl Write,
    inst: &IrInst,
    interner: &Interner,
    types: &Arena<IrType>,
) -> Result<(), fmt::Error> {
    match inst {
        IrInst::Assign { dst, rvalue, .. } => {
            write!(w, "      %{} = ", dst.0)?;
            write_rvalue(w, rvalue, interner, types)?;
            writeln!(w)?;
        }
        IrInst::Store { dst, value, .. } => {
            write!(w, "      store ")?;
            write_place(w, dst)?;
            write!(w, ", ")?;
            write_operand(w, value)?;
            writeln!(w)?;
        }
        IrInst::Goto(label) => {
            writeln!(w, "      goto .l{}", label.0)?;
        }
        IrInst::Branch {
            cond,
            then_label,
            else_label,
            ..
        } => {
            write!(w, "      branch ")?;
            write_operand(w, cond)?;
            writeln!(w, " -> .l{}, .l{}", then_label.0, else_label.0)?;
        }
        IrInst::Switch {
            scrutinee,
            arms,
            default,
            ..
        } => {
            write!(w, "      switch ")?;
            write_operand(w, scrutinee)?;
            writeln!(w, " {{")?;
            for arm in arms {
                write!(w, "        ")?;
                write_const_value(w, &arm.value)?;
                writeln!(w, " -> .l{}", arm.label.0)?;
            }
            writeln!(w, "        _ -> .l{}", default.0)?;
            writeln!(w, "      }}")?;
        }
        IrInst::Label(label) => {
            writeln!(w, "    .l{}:", label.0)?;
        }
        IrInst::Return { value, .. } => match value {
            Some(v) => {
                write!(w, "      return ")?;
                write_operand(w, v)?;
                writeln!(w)?;
            }
            None => writeln!(w, "      return unit")?,
        },
        IrInst::EffectPush {
            effect, handler_fn, ..
        } => {
            writeln!(
                w,
                "      eff.push effect#{} fn#{}",
                effect.0,
                handler_fn.raw()
            )?;
        }
        IrInst::EffectPop { effect, .. } => {
            writeln!(w, "      eff.pop effect#{}", effect.0)?;
        }
        IrInst::EffectDo { op, args, dst, .. } => {
            write!(w, "      %{} = eff.do op#{}", dst.0, op.0)?;
            if !args.is_empty() {
                write!(w, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(w, ", ")?;
                    }
                    write_operand(w, arg)?;
                }
                write!(w, ")")?;
            }
            writeln!(w)?;
        }
        IrInst::EffectResume { value, .. } => {
            write!(w, "      eff.resume ")?;
            write_operand(w, value)?;
            writeln!(w)?;
        }
        IrInst::Nop => {
            writeln!(w, "      nop")?;
        }
    }
    Ok(())
}

fn write_rvalue(
    w: &mut impl Write,
    rv: &IrRvalue,
    interner: &Interner,
    types: &Arena<IrType>,
) -> Result<(), fmt::Error> {
    match rv {
        IrRvalue::Use(op) => write_operand(w, op),
        IrRvalue::Const(v) => write_const_value(w, v),
        IrRvalue::BinOp { op, left, right } => {
            write!(w, "{} ", binop_mnemonic(*op))?;
            write_operand(w, left)?;
            write!(w, ", ")?;
            write_operand(w, right)
        }
        IrRvalue::UnaryOp { op, operand } => {
            write!(w, "{} ", unaryop_mnemonic(*op))?;
            write_operand(w, operand)
        }
        IrRvalue::Call { callee, args, tail } => {
            write_rvalue_call(w, callee, args, *tail, interner)
        }
        IrRvalue::MakeProduct { fields, .. } => write_rvalue_make_product(w, fields),
        IrRvalue::MakeVariant { tag, payload, .. } => write_rvalue_make_variant(w, *tag, payload),
        IrRvalue::MakeArray { elems, .. } => write_rvalue_make_array(w, elems),
        IrRvalue::MakeClosure { fn_id, captures } => {
            write_rvalue_make_closure(w, fn_id.raw(), captures)
        }
        IrRvalue::FieldGet { object, index } => {
            write_operand(w, object)?;
            write!(w, ".{index}")
        }
        IrRvalue::IndexGet { array, index } => {
            write_operand(w, array)?;
            write!(w, "[")?;
            write_operand(w, index)?;
            write!(w, "]")
        }
        IrRvalue::GetTag { value } => {
            write!(w, "get.tag ")?;
            write_operand(w, value)
        }
        IrRvalue::GetPayload { value, tag, field } => {
            write!(w, "get.pay#{tag}.{field} ")?;
            write_operand(w, value)
        }
        IrRvalue::AllocRef { ty } => {
            write!(w, "alc.ref ")?;
            write_type(w, *ty, types)
        }
        IrRvalue::AllocArena { ty } => {
            write!(w, "alc.arn ")?;
            write_type(w, *ty, types)
        }
        IrRvalue::Deref { ptr } => {
            write!(w, "deref ")?;
            write_operand(w, ptr)
        }
        IrRvalue::Cast { operand, from, to } => {
            write!(w, "cast ")?;
            write_operand(w, operand)?;
            write!(w, " : ")?;
            write_type(w, *from, types)?;
            write!(w, " -> ")?;
            write_type(w, *to, types)
        }
        IrRvalue::Spawn { callee, args } => {
            write!(w, "spawn ")?;
            write_callee(w, callee, interner)?;
            write!(w, "(")?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(w, ", ")?;
                }
                write_operand(w, arg)?;
            }
            write!(w, ")")
        }
        IrRvalue::Await { task } => {
            write!(w, "await ")?;
            write_operand(w, task)
        }
    }
}

fn write_rvalue_call(
    w: &mut impl Write,
    callee: &IrCallee,
    args: &[IrOperand],
    tail: bool,
    interner: &Interner,
) -> Result<(), fmt::Error> {
    if tail {
        write!(w, "tail ")?;
    }
    write!(w, "call ")?;
    write_callee(w, callee, interner)?;
    write!(w, "(")?;
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            write!(w, ", ")?;
        }
        write_operand(w, arg)?;
    }
    write!(w, ")")
}

fn write_rvalue_make_product(w: &mut impl Write, fields: &[IrOperand]) -> Result<(), fmt::Error> {
    write!(w, "mk.prd(")?;
    for (i, f) in fields.iter().enumerate() {
        if i > 0 {
            write!(w, ", ")?;
        }
        write_operand(w, f)?;
    }
    write!(w, ")")
}

fn write_rvalue_make_variant(
    w: &mut impl Write,
    tag: u32,
    payload: &[IrOperand],
) -> Result<(), fmt::Error> {
    write!(w, "mk.var #{tag}(")?;
    for (i, p) in payload.iter().enumerate() {
        if i > 0 {
            write!(w, ", ")?;
        }
        write_operand(w, p)?;
    }
    write!(w, ")")
}

fn write_rvalue_make_array(w: &mut impl Write, elems: &[IrOperand]) -> Result<(), fmt::Error> {
    write!(w, "mk.arr[")?;
    for (i, e) in elems.iter().enumerate() {
        if i > 0 {
            write!(w, ", ")?;
        }
        write_operand(w, e)?;
    }
    write!(w, "]")
}

fn write_rvalue_make_closure(
    w: &mut impl Write,
    fn_raw: u32,
    captures: &[IrOperand],
) -> Result<(), fmt::Error> {
    write!(w, "mk.cls fn#{fn_raw}")?;
    if !captures.is_empty() {
        write!(w, "[")?;
        for (i, cap) in captures.iter().enumerate() {
            if i > 0 {
                write!(w, ", ")?;
            }
            write_operand(w, cap)?;
        }
        write!(w, "]")?;
    }
    Ok(())
}

fn write_operand(w: &mut impl Write, op: &IrOperand) -> Result<(), fmt::Error> {
    match op {
        IrOperand::Local(local) => write!(w, "%{}", local.0),
        IrOperand::Const(v) => write_const_value(w, v),
    }
}

fn write_place(w: &mut impl Write, place: &IrPlace) -> Result<(), fmt::Error> {
    match place {
        IrPlace::Local(local) => write!(w, "%{}", local.0),
        IrPlace::Field { base, index } => write!(w, "%{}.{}", base.0, index),
        IrPlace::Index { base, index } => {
            write!(w, "%{}[", base.0)?;
            write_operand(w, index)?;
            write!(w, "]")
        }
        IrPlace::Deref { ptr } => write!(w, "*%{}", ptr.0),
    }
}

fn write_const_value(w: &mut impl Write, v: &IrConstValue) -> Result<(), fmt::Error> {
    match v {
        IrConstValue::Int(n) => write!(w, "const.i {n}"),
        IrConstValue::Float(f) => write!(w, "const.f {f}"),
        IrConstValue::Bool(b) => write!(w, "const.b {b}"),
        IrConstValue::Rune(r) => write!(w, "const.r {r:#010x}"),
        IrConstValue::Str(s) => write!(w, "const.s sym#{}", s.0),
        IrConstValue::Unit => write!(w, "const.unit"),
        IrConstValue::FnRef(id) => write!(w, "const.fn fn#{id}"),
    }
}

fn write_callee(
    w: &mut impl Write,
    callee: &IrCallee,
    interner: &Interner,
) -> Result<(), fmt::Error> {
    match callee {
        IrCallee::Direct(fn_idx) => write!(w, "fn#{}", fn_idx.raw()),
        IrCallee::Indirect(local) => write!(w, "*%{}", local.0),
        IrCallee::Instance {
            class_def,
            method,
            instance_fn,
        } => {
            let method_name = interner.resolve(*method);
            write!(
                w,
                "instance<class#{}, @{method_name}, fn#{}>",
                class_def.0,
                instance_fn.raw()
            )
        }
    }
}

fn write_type(
    w: &mut impl Write,
    idx: Idx<IrType>,
    types: &Arena<IrType>,
) -> Result<(), fmt::Error> {
    let ty = &types[idx];
    match ty {
        IrType::Unit => write!(w, "Unit"),
        IrType::Bool => write!(w, "Bool"),
        IrType::Int8 => write!(w, "Int8"),
        IrType::Int16 => write!(w, "Int16"),
        IrType::Int32 => write!(w, "Int32"),
        IrType::Int64 => write!(w, "Int64"),
        IrType::UInt8 => write!(w, "UInt8"),
        IrType::UInt16 => write!(w, "UInt16"),
        IrType::UInt32 => write!(w, "UInt32"),
        IrType::UInt64 => write!(w, "UInt64"),
        IrType::Float32 => write!(w, "Float32"),
        IrType::Float64 => write!(w, "Float64"),
        IrType::Rune => write!(w, "Rune"),
        IrType::Product { fields } => write_type_product(w, fields, types),
        IrType::Sum { variants } => write_type_sum(w, variants, types),
        IrType::Array { elem } => {
            write!(w, "[")?;
            write_type(w, *elem, types)?;
            write!(w, "]")
        }
        IrType::Fn {
            params,
            ret,
            effect_mask,
        } => write_type_fn(w, params, *ret, *effect_mask, types),
        IrType::Ref { inner } => {
            write!(w, "&")?;
            write_type(w, *inner, types)
        }
        IrType::Ptr { inner } => {
            write!(w, "*")?;
            write_type(w, *inner, types)
        }
        IrType::Closure { fn_ty, env_ty } => {
            write!(w, "closure(")?;
            write_type(w, *fn_ty, types)?;
            write!(w, ", ")?;
            write_type(w, *env_ty, types)?;
            write!(w, ")")
        }
        IrType::TypeParam { index } => write!(w, "T{index}"),
        IrType::WitnessTable { class_def } => write!(w, "witness<class#{}>", class_def.0),
    }
}

fn write_type_product(
    w: &mut impl Write,
    fields: &[Idx<IrType>],
    types: &Arena<IrType>,
) -> Result<(), fmt::Error> {
    write!(w, "(")?;
    for (i, f) in fields.iter().enumerate() {
        if i > 0 {
            write!(w, ", ")?;
        }
        write_type(w, *f, types)?;
    }
    write!(w, ")")
}

fn write_type_sum(
    w: &mut impl Write,
    variants: &[IrSumVariant],
    types: &Arena<IrType>,
) -> Result<(), fmt::Error> {
    for (i, v) in variants.iter().enumerate() {
        if i > 0 {
            write!(w, " | ")?;
        }
        write!(w, "#{i}")?;
        if !v.fields.is_empty() {
            write!(w, "(")?;
            for (j, f) in v.fields.iter().enumerate() {
                if j > 0 {
                    write!(w, ", ")?;
                }
                write_type(w, *f, types)?;
            }
            write!(w, ")")?;
        }
    }
    Ok(())
}

fn write_type_fn(
    w: &mut impl Write,
    params: &[Idx<IrType>],
    ret: Idx<IrType>,
    effect_mask: IrEffectMask,
    types: &Arena<IrType>,
) -> Result<(), fmt::Error> {
    write!(w, "(")?;
    for (i, p) in params.iter().enumerate() {
        if i > 0 {
            write!(w, ", ")?;
        }
        write_type(w, *p, types)?;
    }
    write!(w, ") -> ")?;
    write_type(w, ret, types)?;
    if !effect_mask.is_pure() {
        write!(w, " under {}", format_effect_mask(effect_mask))?;
    }
    Ok(())
}

const fn binop_mnemonic(op: IrBinOp) -> &'static str {
    match op {
        IrBinOp::IAdd => "i.add",
        IrBinOp::ISub => "i.sub",
        IrBinOp::IMul => "i.mul",
        IrBinOp::IDiv => "i.div",
        IrBinOp::IRem => "i.rem",
        IrBinOp::UAdd => "u.add",
        IrBinOp::USub => "u.sub",
        IrBinOp::UMul => "u.mul",
        IrBinOp::UDiv => "u.div",
        IrBinOp::URem => "u.rem",
        IrBinOp::FAdd => "f.add",
        IrBinOp::FSub => "f.sub",
        IrBinOp::FMul => "f.mul",
        IrBinOp::FDiv => "f.div",
        IrBinOp::FRem => "f.rem",
        IrBinOp::IEq => "cmp.eq",
        IrBinOp::INe => "cmp.ne",
        IrBinOp::ILt => "cmp.lt",
        IrBinOp::ILe => "cmp.le",
        IrBinOp::IGt => "cmp.gt",
        IrBinOp::IGe => "cmp.ge",
        IrBinOp::ULt => "cmp.lt.un",
        IrBinOp::ULe => "cmp.le.un",
        IrBinOp::UGt => "cmp.gt.un",
        IrBinOp::UGe => "cmp.ge.un",
        IrBinOp::FEq => "cmp.feq",
        IrBinOp::FNe => "cmp.fne",
        IrBinOp::FLt => "cmp.flt",
        IrBinOp::FLe => "cmp.fle",
        IrBinOp::FGt => "cmp.fgt",
        IrBinOp::FGe => "cmp.fge",
        IrBinOp::And => "b.and",
        IrBinOp::Or => "b.or",
        IrBinOp::Xor => "b.xor",
        IrBinOp::Shl => "b.shl",
        IrBinOp::Shr => "b.shr",
        IrBinOp::ShrUn => "b.shr.un",
    }
}

const fn unaryop_mnemonic(op: IrUnaryOp) -> &'static str {
    match op {
        IrUnaryOp::INeg => "i.neg",
        IrUnaryOp::FNeg => "f.neg",
        IrUnaryOp::Not => "b.not",
    }
}

fn format_effect_mask(mask: IrEffectMask) -> String {
    let names = [
        (0u16, "IO"),
        (1, "Async"),
        (2, "State"),
        (3, "Unsafe"),
        (4, "Manual"),
        (5, "Throw"),
        (6, "Control"),
        (7, "Arena"),
    ];
    let mut parts: Vec<&str> = names
        .iter()
        .filter(|(bit, _)| mask.0 & (1u16 << bit) != 0)
        .map(|(_, name)| *name)
        .collect();
    if parts.is_empty() {
        parts.push("pure");
    }
    parts.join(" | ")
}
