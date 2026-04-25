use super::*;

#[must_use]
pub fn format_hil(module: &HilModule) -> String {
    let mut out = String::new();
    writeln!(&mut out, "module {} {{", module.name).expect("write to string");
    for function in &module.functions {
        format_function(&mut out, function);
    }
    out.push_str("}\n");
    out
}

fn format_function(out: &mut String, function: &HilFunction) {
    write!(out, "  fn {}(", function.name).expect("write to string");
    for (index, param) in function.params.iter().enumerate() {
        if index != 0 {
            out.push_str(", ");
        }
        write!(out, "{} {}: {}", param.id, param.name, param.ty).expect("write to string");
    }
    out.push(')');
    if let Some(result_ty) = &function.result_ty {
        write!(out, " -> {result_ty}").expect("write to string");
    }
    if !function.capabilities.is_empty() {
        out.push_str(" capabilities [");
        for (index, capability) in function.capabilities.iter().enumerate() {
            if index != 0 {
                out.push_str(", ");
            }
            write!(out, "{capability}").expect("write to string");
        }
        out.push(']');
    }
    out.push_str(" {\n");
    for block in &function.blocks {
        writeln!(out, "    block {}:", block.name).expect("write to string");
        for instruction in &block.instructions {
            format_instruction(out, instruction);
        }
        format_terminator(out, &block.terminator);
    }
    out.push_str("  }\n");
}

fn format_instruction(out: &mut String, instruction: &HilInstruction) {
    match instruction {
        HilInstruction::ConstInt { out: id, ty, value } => {
            writeln!(out, "      {id}: {ty} = const.int {value}").expect("write to string");
        }
        HilInstruction::Binary {
            out: id,
            op,
            ty,
            left,
            right,
        } => {
            writeln!(out, "      {id}: {ty} = {op} {left}, {right}").expect("write to string");
        }
        HilInstruction::Call {
            out: Some(id),
            result_ty: Some(ty),
            callee,
            args,
        } => {
            write!(out, "      {id}: {ty} = call {callee}(").expect("write to string");
            format_value_list(out, args);
            out.push_str(")\n");
        }
        HilInstruction::Call { callee, args, .. } => {
            write!(out, "      call {callee}(").expect("write to string");
            format_value_list(out, args);
            out.push_str(")\n");
        }
        HilInstruction::NewObj {
            out: id,
            ty,
            variant,
            fields,
        } => {
            write!(out, "      {id}: {ty} = data.new .{variant}(").expect("write to string");
            format_value_list(out, fields);
            out.push_str(")\n");
        }
        HilInstruction::EffectCall {
            out: id,
            result_ty,
            effect,
            op,
            args,
        } => {
            write!(out, "      {id}: {result_ty} = effect.call {effect}.{op}(")
                .expect("write to string");
            format_value_list(out, args);
            out.push_str(")\n");
        }
        HilInstruction::ForeignCall {
            out: Some(id),
            result_ty: Some(ty),
            foreign,
            args,
        } => {
            write!(out, "      {id}: {ty} = native.call {foreign}(").expect("write to string");
            format_value_list(out, args);
            out.push_str(")\n");
        }
        HilInstruction::ForeignCall { foreign, args, .. } => {
            write!(out, "      native.call {foreign}(").expect("write to string");
            format_value_list(out, args);
            out.push_str(")\n");
        }
    }
}

fn format_terminator(out: &mut String, terminator: &HilTerminator) {
    match terminator {
        HilTerminator::Return(Some(id)) => {
            writeln!(out, "      return {id}").expect("write to string");
        }
        HilTerminator::Return(None) => out.push_str("      return\n"),
        HilTerminator::Branch { target } => {
            writeln!(out, "      branch {target}").expect("write to string");
        }
        HilTerminator::CondBranch {
            condition,
            then_target,
            else_target,
        } => {
            writeln!(
                out,
                "      branch.if {condition}, {then_target}, {else_target}"
            )
            .expect("write to string");
        }
        HilTerminator::TailCall { callee, args } => {
            write!(out, "      tail.call {callee}(").expect("write to string");
            format_value_list(out, args);
            out.push_str(")\n");
        }
    }
}

fn format_value_list(out: &mut String, values: &[HilValueId]) {
    for (index, id) in values.iter().enumerate() {
        if index != 0 {
            out.push_str(", ");
        }
        write!(out, "{id}").expect("write to string");
    }
}
