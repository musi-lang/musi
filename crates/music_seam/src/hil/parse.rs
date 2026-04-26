use super::*;

/// Parses HIL text into a HIL module.
///
/// # Errors
///
/// Returns [`AssemblyError`] if the text is not valid HIL.
pub fn parse_hil(text: &str) -> Result<HilModule, AssemblyError> {
    let mut lines = text.lines().enumerate().peekable();
    let (module_name, mut functions) = parse_module_header(&mut lines)?;
    while let Some((_, raw)) = lines.peek() {
        let line = raw.trim();
        if line.is_empty() {
            let _ = lines.next();
            continue;
        }
        if line == "}" {
            let _ = lines.next();
            break;
        }
        functions.push(parse_function(&mut lines)?);
    }
    Ok(HilModule::new(module_name, functions.into_boxed_slice()))
}

fn parse_module_header(
    lines: HilLineCursorRef<'_, '_>,
) -> Result<(Box<str>, Vec<HilFunction>), AssemblyError> {
    while let Some((_, raw)) = lines.peek() {
        if raw.trim().is_empty() {
            let _ = lines.next();
            continue;
        }
        break;
    }
    let Some((line_no, raw)) = lines.next() else {
        return Err(AssemblyError::text_parse_source("empty HIL text"));
    };
    let line = raw.trim();
    let Some(rest) = line.strip_prefix("module ") else {
        return Err(AssemblyError::text_parse_source(format!(
            "line {}: module header expected",
            line_no + 1
        )));
    };
    let Some(name) = rest.strip_suffix(" {") else {
        return Err(AssemblyError::text_parse_source(format!(
            "line {}: malformed module header",
            line_no + 1
        )));
    };
    Ok((name.trim().into(), Vec::new()))
}

fn parse_function(lines: HilLineCursorRef<'_, '_>) -> Result<HilFunction, AssemblyError> {
    let (header_line_no, header) = lines
        .next()
        .ok_or_else(|| AssemblyError::text_parse_source("function header expected"))?;
    let header = header.trim();
    let Some(head) = header
        .strip_prefix("fn ")
        .or_else(|| header.strip_prefix("  fn "))
    else {
        return Err(AssemblyError::text_parse_source(format!(
            "line {}: function header expected",
            header_line_no + 1
        )));
    };
    let Some((name_part, after_name)) = head.split_once('(') else {
        return Err(AssemblyError::text_parse_source(format!(
            "line {}: malformed function header",
            header_line_no + 1
        )));
    };
    let name = name_part.trim().to_owned();
    let Some((params_text, mut tail)) = after_name.split_once(')') else {
        return Err(AssemblyError::text_parse_source(format!(
            "line {}: malformed parameter list",
            header_line_no + 1
        )));
    };
    let params = parse_params(params_text.trim())?;
    let mut result_ty = None::<HilType>;
    let mut capabilities = Vec::<HilShape>::new();
    tail = tail.trim();
    if let Some(after_arrow) = tail.strip_prefix("-> ") {
        let split_at = after_arrow
            .find(" capabilities [")
            .or_else(|| after_arrow.find(" {"));
        let idx = split_at.ok_or_else(|| {
            AssemblyError::text_parse_source(format!(
                "line {}: malformed function result type",
                header_line_no + 1
            ))
        })?;
        let (result_text, remaining_tail) = after_arrow.split_at(idx);
        result_ty = Some(HilType::new(result_text.trim()));
        tail = remaining_tail.trim();
    }
    if let Some(after_capabilities) = tail.strip_prefix("capabilities [") {
        let Some((caps_text, after_caps)) = after_capabilities.split_once(']') else {
            return Err(AssemblyError::text_parse_source(format!(
                "line {}: malformed capability list",
                header_line_no + 1
            )));
        };
        capabilities = parse_capabilities(caps_text.trim())?;
        tail = after_caps.trim();
    }
    if tail != "{" {
        return Err(AssemblyError::text_parse_source(format!(
            "line {}: function body opener expected",
            header_line_no + 1
        )));
    }

    let mut blocks = Vec::<HilBlock>::new();
    while let Some((_, raw)) = lines.peek() {
        let line = raw.trim();
        if line.is_empty() {
            let _ = lines.next();
            continue;
        }
        if line == "}" {
            let _ = lines.next();
            break;
        }
        blocks.push(parse_block(lines)?);
    }
    Ok(HilFunction::new(name, params, result_ty, blocks).with_capabilities(capabilities))
}

fn parse_block(lines: &mut HilLineCursor<'_>) -> Result<HilBlock, AssemblyError> {
    let (line_no, block_header_raw) = lines
        .next()
        .ok_or_else(|| AssemblyError::text_parse_source("block header expected"))?;
    let block_header = block_header_raw.trim();
    let block_name = if let Some(name) = block_header
        .strip_prefix("block ")
        .and_then(|v| v.strip_suffix(':'))
    {
        name.trim().to_owned()
    } else {
        return Err(AssemblyError::text_parse_source(format!(
            "line {}: block header expected",
            line_no + 1
        )));
    };
    let mut instructions = Vec::<HilInstruction>::new();
    let terminator: HilTerminator;
    loop {
        let (line_no, raw) = lines
            .next()
            .ok_or_else(|| AssemblyError::text_parse_source("block terminator expected"))?;
        let line = raw.trim();
        if line.is_empty() {
            continue;
        }
        if let Some(term) = parse_terminator(line)? {
            terminator = term;
            break;
        }
        let instr =
            parse_instruction(line).map_err(|msg| hil_parse_line_error(line_no, msg.as_ref()))?;
        instructions.push(instr);
    }
    Ok(HilBlock::new(block_name, instructions, terminator))
}

fn parse_params(text: &str) -> Result<Vec<HilParam>, AssemblyError> {
    if text.is_empty() {
        return Ok(Vec::new());
    }
    text.split(',')
        .map(|part| {
            let part = part.trim();
            let Some((left, ty_text)) = part.split_once(':') else {
                return Err(AssemblyError::text_parse_source(
                    "parameter type separator `:` missing",
                ));
            };
            let mut left_parts = left.split_whitespace();
            let id_text = left_parts
                .next()
                .ok_or_else(|| AssemblyError::text_parse_source("parameter value id missing"))?;
            let name_text = left_parts
                .next()
                .ok_or_else(|| AssemblyError::text_parse_source("parameter name missing"))?;
            Ok(HilParam::new(
                name_text,
                parse_value_id(id_text)?,
                HilType::new(ty_text.trim()),
            ))
        })
        .collect()
}

fn parse_capabilities(text: &str) -> Result<Vec<HilShape>, AssemblyError> {
    if text.is_empty() {
        return Ok(Vec::new());
    }
    text.split(',')
        .map(|name| match name.trim() {
            "effect" => Ok(HilShape::Effect),
            "native" => Ok(HilShape::Native),
            "syntax" => Ok(HilShape::Syntax),
            "known" => Ok(HilShape::Known),
            other => Err(AssemblyError::text_parse_source(format!(
                "unknown shape `{other}`"
            ))),
        })
        .collect()
}

fn parse_instruction(line: &str) -> Result<HilInstruction, Box<str>> {
    if let Some(rest) = line.strip_prefix("call ")
        && let Some((callee, args)) = parse_named_call(rest)
    {
        return Ok(HilInstruction::Call {
            out: None,
            result_ty: None,
            callee: callee.into(),
            args: parse_value_id_list(args).map_err(|_| hil_parse_error("call args malformed"))?,
        });
    }
    if let Some(rest) = line.strip_prefix("native.call ")
        && let Some((foreign, args)) = parse_named_call(rest)
    {
        return Ok(HilInstruction::ForeignCall {
            out: None,
            result_ty: None,
            foreign: foreign.into(),
            args: parse_value_id_list(args)
                .map_err(|_| hil_parse_error("native.call args malformed"))?,
        });
    }
    let Some((lhs, rhs)) = line.split_once(" = ") else {
        return Err(hil_parse_error("instruction assignment `=` missing"));
    };
    let Some((out_text, ty_text)) = lhs.split_once(':') else {
        return Err(hil_parse_error(
            "instruction lhs type annotation `:` missing",
        ));
    };
    let out = parse_value_id(out_text.trim())
        .map_err(|_| hil_parse_error("output value id malformed"))?;
    let ty = HilType::new(ty_text.trim());
    parse_assigned_instruction(out, ty, rhs)
}

fn parse_assigned_instruction(
    out: HilValueId,
    ty: HilType,
    rhs: &str,
) -> Result<HilInstruction, Box<str>> {
    if let Some(value_text) = rhs.strip_prefix("const.int ") {
        let int_value = value_text
            .trim()
            .parse::<i64>()
            .map_err(|_| hil_parse_error("const.int value malformed"))?;
        return Ok(HilInstruction::ConstInt {
            out,
            ty,
            value: int_value,
        });
    }
    if let Some(rest) = rhs.strip_prefix("call ")
        && let Some((callee, args)) = parse_named_call(rest)
    {
        return Ok(HilInstruction::Call {
            out: Some(out),
            result_ty: Some(ty),
            callee: callee.into(),
            args: parse_value_id_list(args).map_err(|_| hil_parse_error("call args malformed"))?,
        });
    }
    if let Some(rest) = rhs.strip_prefix("native.call ")
        && let Some((foreign, args)) = parse_named_call(rest)
    {
        return Ok(HilInstruction::ForeignCall {
            out: Some(out),
            result_ty: Some(ty),
            foreign: foreign.into(),
            args: parse_value_id_list(args)
                .map_err(|_| hil_parse_error("native.call args malformed"))?,
        });
    }
    if let Some(rest) = rhs.strip_prefix("effect.call ")
        && let Some((head, args)) = parse_named_call(rest)
        && let Some((effect, op)) = head.split_once('.')
    {
        return Ok(HilInstruction::EffectCall {
            out,
            result_ty: ty,
            effect: effect.into(),
            op: op.into(),
            args: parse_value_id_list(args)
                .map_err(|_| hil_parse_error("effect.call args malformed"))?,
        });
    }
    if let Some(rest) = rhs.strip_prefix("data.new .")
        && let Some((variant, args)) = parse_named_call(rest)
    {
        return Ok(HilInstruction::NewObj {
            out,
            ty,
            variant: variant.into(),
            fields: parse_value_id_list(args)
                .map_err(|_| hil_parse_error("data.new args malformed"))?,
        });
    }
    for (op_text, op) in [
        ("int.add", HilBinaryOp::IntAdd),
        ("int.sub", HilBinaryOp::IntSub),
        ("int.mul", HilBinaryOp::IntMul),
        ("int.eq", HilBinaryOp::IntEq),
    ] {
        if let Some(rest) = rhs.strip_prefix(op_text)
            && let Some(rest) = rest.strip_prefix(' ')
            && let Some((left_text, right_text)) = rest.split_once(',')
        {
            return Ok(HilInstruction::Binary {
                out,
                op,
                ty,
                left: parse_value_id(left_text.trim())
                    .map_err(|_| hil_parse_error("binary left operand malformed"))?,
                right: parse_value_id(right_text.trim())
                    .map_err(|_| hil_parse_error("binary right operand malformed"))?,
            });
        }
    }
    Err(hil_parse_error("unknown instruction"))
}

fn hil_parse_error(message: impl Into<Box<str>>) -> Box<str> {
    message.into()
}

fn hil_parse_line_error(line_no: usize, message: &str) -> AssemblyError {
    AssemblyError::text_parse_source(format!("line {}: {message}", line_no + 1))
}

fn parse_terminator(line: &str) -> Result<Option<HilTerminator>, AssemblyError> {
    if line == "return" {
        return Ok(Some(HilTerminator::Return(None)));
    }
    if let Some(value_text) = line.strip_prefix("return ") {
        return Ok(Some(HilTerminator::Return(Some(parse_value_id(
            value_text.trim(),
        )?))));
    }
    if let Some(target) = line.strip_prefix("branch ") {
        return Ok(Some(HilTerminator::Branch {
            target: target.trim().into(),
        }));
    }
    if let Some(rest) = line.strip_prefix("branch.if ")
        && let Some((cond_text, tail)) = rest.split_once(',')
        && let Some((then_text, else_text)) = tail.split_once(',')
    {
        return Ok(Some(HilTerminator::CondBranch {
            condition: parse_value_id(cond_text.trim())?,
            then_target: then_text.trim().into(),
            else_target: else_text.trim().into(),
        }));
    }
    if let Some(rest) = line.strip_prefix("tail.call ")
        && let Some((callee, args)) = parse_named_call(rest)
    {
        return Ok(Some(HilTerminator::TailCall {
            callee: callee.into(),
            args: parse_value_id_list(args)?,
        }));
    }
    Ok(None)
}

fn parse_named_call(text: &str) -> Option<(&str, &str)> {
    let (name, rest) = text.split_once('(')?;
    let args = rest.strip_suffix(')')?;
    Some((name.trim(), args.trim()))
}

fn parse_value_id_list(text: &str) -> Result<Box<[HilValueId]>, AssemblyError> {
    if text.is_empty() {
        return Ok(Box::new([]));
    }
    text.split(',')
        .map(|part| parse_value_id(part.trim()))
        .collect::<Result<Vec<_>, _>>()
        .map(Vec::into_boxed_slice)
}

fn parse_value_id(text: &str) -> Result<HilValueId, AssemblyError> {
    let number = text
        .strip_prefix('%')
        .ok_or_else(|| {
            AssemblyError::text_parse_source(format!("value id `%n` expected, found `{text}`"))
        })?
        .parse::<u32>()
        .map_err(|_| AssemblyError::text_parse_source(format!("invalid value id `{text}`")))?;
    Ok(HilValueId(number))
}
