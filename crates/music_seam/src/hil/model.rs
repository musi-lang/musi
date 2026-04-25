use super::verify::{verify_instruction, verify_terminator};
use super::*;

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
