use std::collections::{BTreeMap, BTreeSet};
use std::error::Error;
use std::fmt::{self, Display, Formatter, Result as FmtResult, Write as _};
use std::iter::{Enumerate, Peekable};
use std::str::Lines;

use music_base::diag::{CatalogDiagnostic, DiagContext};

use crate::{AssemblyError, SeamDiagKind, diag::hil_verify_error_kind};

type HilName = Box<str>;
type HilTypeMap = BTreeMap<HilValueId, HilType>;
type HilBlockSet = BTreeSet<HilName>;
type HilLineCursor<'a> = Peekable<Enumerate<Lines<'a>>>;
type HilLineCursorRef<'cursor, 'text> = &'cursor mut HilLineCursor<'text>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HilValueId(pub u32);

impl Display for HilValueId {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "%{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HilType(pub HilName);

impl HilType {
    #[must_use]
    pub fn new(name: impl Into<HilName>) -> Self {
        Self(name.into())
    }
}

impl Display for HilType {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.write_str(&self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HilShape {
    Effect,
    Native,
    Syntax,
    Comptime,
}

impl Display for HilShape {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::Effect => f.write_str("effect"),
            Self::Native => f.write_str("native"),
            Self::Syntax => f.write_str("syntax"),
            Self::Comptime => f.write_str("comptime"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HilParam {
    pub id: HilValueId,
    pub name: HilName,
    pub ty: HilType,
}

impl HilParam {
    #[must_use]
    pub fn new(name: impl Into<HilName>, id: HilValueId, ty: HilType) -> Self {
        Self {
            id,
            name: name.into(),
            ty,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HilModule {
    pub name: HilName,
    pub functions: Box<[HilFunction]>,
}

impl HilModule {
    #[must_use]
    pub fn new(name: impl Into<HilName>, functions: impl Into<Box<[HilFunction]>>) -> Self {
        Self {
            name: name.into(),
            functions: functions.into(),
        }
    }

    /// Verifies all functions in this module.
    ///
    /// # Errors
    ///
    /// Returns [`HilVerifyError`] when the HIL violates typed block/value rules.
    pub fn verify(&self) -> HilVerifyResult {
        for function in &self.functions {
            function.verify()?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HilFunction {
    pub name: HilName,
    pub params: Box<[HilParam]>,
    pub result_ty: Option<HilType>,
    pub capabilities: Box<[HilShape]>,
    pub blocks: Box<[HilBlock]>,
}

impl HilFunction {
    #[must_use]
    pub fn new(
        name: impl Into<HilName>,
        params: impl Into<Box<[HilParam]>>,
        result_ty: Option<HilType>,
        blocks: impl Into<Box<[HilBlock]>>,
    ) -> Self {
        Self {
            name: name.into(),
            params: params.into(),
            result_ty,
            capabilities: Box::new([]),
            blocks: blocks.into(),
        }
    }

    #[must_use]
    pub fn with_capabilities(mut self, capabilities: impl Into<Box<[HilShape]>>) -> Self {
        self.capabilities = capabilities.into();
        self
    }

    /// Verifies this function.
    ///
    /// # Errors
    ///
    /// Returns [`HilVerifyError`] when blocks, values, types, or capabilities are invalid.
    pub fn verify(&self) -> HilVerifyResult {
        let block_names = self.verify_block_names()?;
        let mut type_map = self.param_types()?;
        for block in &self.blocks {
            for instruction in &block.instructions {
                verify_instruction(instruction, &self.capabilities, &mut type_map)?;
            }
            verify_terminator(
                &block.terminator,
                self.result_ty.as_ref(),
                &block_names,
                &type_map,
            )?;
        }
        Ok(())
    }

    fn verify_block_names(&self) -> Result<HilBlockSet, HilVerifyError> {
        if self.blocks.is_empty() {
            return Err(HilVerifyError::MissingEntryBlock {
                function: clone_hil_name(&self.name),
            });
        }
        let mut names = HilBlockSet::new();
        for block in &self.blocks {
            if !names.insert(clone_hil_name(&block.name)) {
                return Err(HilVerifyError::DuplicateBlock {
                    function: clone_hil_name(&self.name),
                    block: clone_hil_name(&block.name),
                });
            }
        }
        Ok(names)
    }

    fn param_types(&self) -> Result<HilTypeMap, HilVerifyError> {
        let mut types = HilTypeMap::new();
        for param in &self.params {
            if types.insert(param.id, clone_hil_type(&param.ty)).is_some() {
                return Err(HilVerifyError::DuplicateValue { value: param.id });
            }
        }
        Ok(types)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HilBlock {
    pub name: HilName,
    pub instructions: Box<[HilInstruction]>,
    pub terminator: HilTerminator,
}

impl HilBlock {
    #[must_use]
    pub fn new(
        name: impl Into<HilName>,
        instructions: impl Into<Box<[HilInstruction]>>,
        terminator: HilTerminator,
    ) -> Self {
        Self {
            name: name.into(),
            instructions: instructions.into(),
            terminator,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HilBinaryOp {
    IntAdd,
    IntSub,
    IntMul,
    IntEq,
}

impl Display for HilBinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::IntAdd => f.write_str("int.add"),
            Self::IntSub => f.write_str("int.sub"),
            Self::IntMul => f.write_str("int.mul"),
            Self::IntEq => f.write_str("int.eq"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HilInstruction {
    ConstInt {
        out: HilValueId,
        ty: HilType,
        value: i64,
    },
    Binary {
        out: HilValueId,
        op: HilBinaryOp,
        ty: HilType,
        left: HilValueId,
        right: HilValueId,
    },
    Call {
        out: Option<HilValueId>,
        result_ty: Option<HilType>,
        callee: HilName,
        args: Box<[HilValueId]>,
    },
    NewObj {
        out: HilValueId,
        ty: HilType,
        variant: HilName,
        fields: Box<[HilValueId]>,
    },
    EffectCall {
        out: HilValueId,
        result_ty: HilType,
        effect: HilName,
        op: HilName,
        args: Box<[HilValueId]>,
    },
    ForeignCall {
        out: Option<HilValueId>,
        result_ty: Option<HilType>,
        foreign: HilName,
        args: Box<[HilValueId]>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HilTerminator {
    Return(Option<HilValueId>),
    Branch {
        target: HilName,
    },
    CondBranch {
        condition: HilValueId,
        then_target: HilName,
        else_target: HilName,
    },
    TailCall {
        callee: HilName,
        args: Box<[HilValueId]>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HilVerifyError {
    MissingEntryBlock {
        function: HilName,
    },
    DuplicateBlock {
        function: HilName,
        block: HilName,
    },
    DuplicateValue {
        value: HilValueId,
    },
    UndefinedValue {
        value: HilValueId,
    },
    TypeMismatch {
        value: HilValueId,
        expected: HilType,
        found: HilType,
    },
    MissingBlockTarget {
        target: HilName,
    },
    ReturnTypeMismatch {
        expected: HilType,
        found: HilType,
    },
    ReturnValueMissing {
        expected: HilType,
    },
    ReturnValueUnexpected,
    ShapeRequired {
        capability: HilShape,
    },
}

impl HilVerifyError {
    #[must_use]
    pub fn diagnostic(&self) -> CatalogDiagnostic<SeamDiagKind> {
        CatalogDiagnostic::new(self.diag_kind(), self.diag_context())
    }

    const fn diag_kind(&self) -> SeamDiagKind {
        hil_verify_error_kind(self)
    }

    fn diag_context(&self) -> DiagContext {
        match self {
            Self::MissingEntryBlock { function } => DiagContext::new().with("function", function),
            Self::DuplicateBlock { function, block } => DiagContext::new()
                .with("function", function)
                .with("block", block),
            Self::DuplicateValue { value } | Self::UndefinedValue { value } => {
                DiagContext::new().with("value", value)
            }
            Self::TypeMismatch {
                value,
                expected,
                found,
            } => DiagContext::new()
                .with("value", value)
                .with("expected", expected)
                .with("found", found),
            Self::MissingBlockTarget { target } => DiagContext::new().with("target", target),
            Self::ReturnTypeMismatch { expected, found } => DiagContext::new()
                .with("expected", expected)
                .with("found", found),
            Self::ReturnValueMissing { expected } => DiagContext::new().with("expected", expected),
            Self::ShapeRequired { capability } => DiagContext::new().with("shape", capability),
            Self::ReturnValueUnexpected => DiagContext::new(),
        }
    }
}

impl Display for HilVerifyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.diagnostic(), f)
    }
}

impl Error for HilVerifyError {}

pub type HilVerifyResult<T = ()> = Result<T, HilVerifyError>;

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

/// Parses public HIL text format produced by [`format_hil`].
///
/// # Errors
///
/// Returns [`AssemblyError::TextParseFailed`] when syntax is invalid.
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
            "comptime" => Ok(HilShape::Comptime),
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

fn clone_hil_name(name: &str) -> HilName {
    name.to_owned().into_boxed_str()
}

fn clone_hil_type(ty: &HilType) -> HilType {
    ty.clone()
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

fn verify_instruction(
    instruction: &HilInstruction,
    capabilities: &[HilShape],
    type_map: &mut HilTypeMap,
) -> HilVerifyResult {
    match instruction {
        HilInstruction::ConstInt { out, ty, .. } => define_value(type_map, *out, ty.clone()),
        HilInstruction::Binary {
            out,
            ty,
            left,
            right,
            ..
        } => {
            expect_type(type_map, *left, ty)?;
            expect_type(type_map, *right, ty)?;
            define_value(type_map, *out, ty.clone())
        }
        HilInstruction::Call {
            out,
            result_ty,
            args,
            ..
        } => {
            expect_defined_many(type_map, args)?;
            define_optional_value(type_map, *out, result_ty.clone())
        }
        HilInstruction::NewObj {
            out, ty, fields, ..
        } => {
            expect_defined_many(type_map, fields)?;
            define_value(type_map, *out, ty.clone())
        }
        HilInstruction::EffectCall {
            out,
            result_ty,
            args,
            ..
        } => {
            require_capability(capabilities, HilShape::Effect)?;
            expect_defined_many(type_map, args)?;
            define_value(type_map, *out, result_ty.clone())
        }
        HilInstruction::ForeignCall {
            out,
            result_ty,
            args,
            ..
        } => {
            require_capability(capabilities, HilShape::Native)?;
            expect_defined_many(type_map, args)?;
            define_optional_value(type_map, *out, result_ty.clone())
        }
    }
}

fn verify_terminator(
    terminator: &HilTerminator,
    result_ty: Option<&HilType>,
    block_names: &HilBlockSet,
    type_map: &HilTypeMap,
) -> HilVerifyResult {
    match terminator {
        HilTerminator::Return(Some(id)) => {
            let found = type_of(type_map, *id)?;
            let Some(expected) = result_ty else {
                return Err(HilVerifyError::ReturnValueUnexpected);
            };
            if found == *expected {
                Ok(())
            } else {
                Err(HilVerifyError::ReturnTypeMismatch {
                    expected: expected.clone(),
                    found,
                })
            }
        }
        HilTerminator::Return(None) => result_ty.map_or(Ok(()), |expected| {
            Err(HilVerifyError::ReturnValueMissing {
                expected: expected.clone(),
            })
        }),
        HilTerminator::Branch { target } => verify_target(block_names, target),
        HilTerminator::CondBranch {
            condition,
            then_target,
            else_target,
        } => {
            let _ = type_of(type_map, *condition)?;
            verify_target(block_names, then_target)?;
            verify_target(block_names, else_target)
        }
        HilTerminator::TailCall { args, .. } => expect_defined_many(type_map, args),
    }
}

fn define_optional_value(
    type_map: &mut HilTypeMap,
    id: Option<HilValueId>,
    ty: Option<HilType>,
) -> HilVerifyResult {
    match (id, ty) {
        (Some(id), Some(ty)) => define_value(type_map, id, ty),
        _ => Ok(()),
    }
}

fn define_value(type_map: &mut HilTypeMap, id: HilValueId, ty: HilType) -> HilVerifyResult {
    if type_map.insert(id, ty).is_some() {
        Err(HilVerifyError::DuplicateValue { value: id })
    } else {
        Ok(())
    }
}

fn expect_defined_many(type_map: &HilTypeMap, ids: &[HilValueId]) -> HilVerifyResult {
    for id in ids {
        let _ = type_of(type_map, *id)?;
    }
    Ok(())
}

fn expect_type(type_map: &HilTypeMap, id: HilValueId, expected: &HilType) -> HilVerifyResult {
    let found = type_of(type_map, id)?;
    if &found == expected {
        Ok(())
    } else {
        Err(HilVerifyError::TypeMismatch {
            value: id,
            expected: expected.clone(),
            found,
        })
    }
}

fn type_of(type_map: &HilTypeMap, id: HilValueId) -> Result<HilType, HilVerifyError> {
    type_map
        .get(&id)
        .cloned()
        .ok_or(HilVerifyError::UndefinedValue { value: id })
}

fn require_capability(capabilities: &[HilShape], required: HilShape) -> HilVerifyResult {
    if capabilities.contains(&required) {
        Ok(())
    } else {
        Err(HilVerifyError::ShapeRequired {
            capability: required,
        })
    }
}

fn verify_target(block_names: &HilBlockSet, target: &str) -> HilVerifyResult {
    if block_names.contains(target) {
        Ok(())
    } else {
        Err(HilVerifyError::MissingBlockTarget {
            target: target.into(),
        })
    }
}
