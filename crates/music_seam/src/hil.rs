use std::collections::{BTreeMap, BTreeSet};
use std::error::Error;
use std::fmt::{self, Display, Formatter, Result as FmtResult, Write as _};

use music_base::diag::{CatalogDiagnostic, DiagContext};

use crate::{SeamDiagKind, diag::hil_verify_error_kind};

type HilName = Box<str>;
type HilTypeMap = BTreeMap<HilValueId, HilType>;
type HilBlockSet = BTreeSet<HilName>;

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
pub enum HilCapability {
    Effect,
    Ffi,
    Syntax,
    Comptime,
}

impl Display for HilCapability {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::Effect => f.write_str("effect"),
            Self::Ffi => f.write_str("ffi"),
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
    pub capabilities: Box<[HilCapability]>,
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
    pub fn with_capabilities(mut self, capabilities: impl Into<Box<[HilCapability]>>) -> Self {
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
                function: self.name.clone(),
            });
        }
        let mut names = HilBlockSet::new();
        for block in &self.blocks {
            if !names.insert(block.name.clone()) {
                return Err(HilVerifyError::DuplicateBlock {
                    function: self.name.clone(),
                    block: block.name.clone(),
                });
            }
        }
        Ok(names)
    }

    fn param_types(&self) -> Result<HilTypeMap, HilVerifyError> {
        let mut types = HilTypeMap::new();
        for param in &self.params {
            if types.insert(param.id, param.ty.clone()).is_some() {
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
    DataNew {
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
    CapabilityRequired {
        capability: HilCapability,
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
            Self::CapabilityRequired { capability } => {
                DiagContext::new().with("capability", capability)
            }
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
    writeln!(&mut out, "module @{} {{", module.name).expect("write to string");
    for function in &module.functions {
        format_function(&mut out, function);
    }
    out.push_str("}\n");
    out
}

fn format_function(out: &mut String, function: &HilFunction) {
    write!(out, "  fn @{}(", function.name).expect("write to string");
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
        out.push_str(" capabilities(");
        for (index, capability) in function.capabilities.iter().enumerate() {
            if index != 0 {
                out.push_str(", ");
            }
            write!(out, "{capability}").expect("write to string");
        }
        out.push(')');
    }
    out.push_str(" {\n");
    for block in &function.blocks {
        writeln!(out, "  {}:", block.name).expect("write to string");
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
            writeln!(out, "    {id}: {ty} = const.int {value}").expect("write to string");
        }
        HilInstruction::Binary {
            out: id,
            op,
            ty,
            left,
            right,
        } => {
            writeln!(out, "    {id}: {ty} = {op} {left}, {right}").expect("write to string");
        }
        HilInstruction::Call {
            out: Some(id),
            result_ty: Some(ty),
            callee,
            args,
        } => {
            write!(out, "    {id}: {ty} = call @{callee}(").expect("write to string");
            format_value_list(out, args);
            out.push_str(")\n");
        }
        HilInstruction::Call { callee, args, .. } => {
            write!(out, "    call @{callee}(").expect("write to string");
            format_value_list(out, args);
            out.push_str(")\n");
        }
        HilInstruction::DataNew {
            out: id,
            ty,
            variant,
            fields,
        } => {
            write!(out, "    {id}: {ty} = data.new .{variant}(").expect("write to string");
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
            write!(out, "    {id}: {result_ty} = effect.call @{effect}.{op}(")
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
            write!(out, "    {id}: {ty} = ffi.call @{foreign}(").expect("write to string");
            format_value_list(out, args);
            out.push_str(")\n");
        }
        HilInstruction::ForeignCall { foreign, args, .. } => {
            write!(out, "    ffi.call @{foreign}(").expect("write to string");
            format_value_list(out, args);
            out.push_str(")\n");
        }
    }
}

fn format_terminator(out: &mut String, terminator: &HilTerminator) {
    match terminator {
        HilTerminator::Return(Some(id)) => {
            writeln!(out, "    return {id}").expect("write to string");
        }
        HilTerminator::Return(None) => out.push_str("    return\n"),
        HilTerminator::Branch { target } => {
            writeln!(out, "    branch {target}").expect("write to string");
        }
        HilTerminator::CondBranch {
            condition,
            then_target,
            else_target,
        } => {
            writeln!(
                out,
                "    branch.if {condition}, {then_target}, {else_target}"
            )
            .expect("write to string");
        }
        HilTerminator::TailCall { callee, args } => {
            write!(out, "    tail.call @{callee}(").expect("write to string");
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
    capabilities: &[HilCapability],
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
        HilInstruction::DataNew {
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
            require_capability(capabilities, HilCapability::Effect)?;
            expect_defined_many(type_map, args)?;
            define_value(type_map, *out, result_ty.clone())
        }
        HilInstruction::ForeignCall {
            out,
            result_ty,
            args,
            ..
        } => {
            require_capability(capabilities, HilCapability::Ffi)?;
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

fn require_capability(capabilities: &[HilCapability], required: HilCapability) -> HilVerifyResult {
    if capabilities.contains(&required) {
        Ok(())
    } else {
        Err(HilVerifyError::CapabilityRequired {
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
