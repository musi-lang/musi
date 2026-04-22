use music_base::diag::{Diag, DiagCode, DiagContext, DiagLevel, DiagnosticKind};

#[path = "diag_catalog_gen.rs"]
mod diag_catalog_gen;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemaDiagKind {
    AttrDuplicateRepr,
    AttrReprRequiresKindString,
    AttrLayoutArgRequiresName,
    AttrDuplicateLayoutAlign,
    AttrLayoutAlignRequiresU32,
    AttrDuplicateLayoutPack,
    AttrLayoutPackRequiresU32,
    AttrUnknownArg,
    AttrKnownRequiresPlainBindLet,
    AttrKnownRequiresNameString,
    AttrKnownRequiresFoundationModule,
    AttrKnownRequiresExport,
    AttrKnownUnknownName,
    AttrIntrinsicRequiresNameString,
    AttrIntrinsicRequiresIntrinsicsModule,
    AttrIntrinsicRequiresForeignLet,
    AttrLinkRequiresForeignLet,
    AttrDataLayoutRequiresDataTarget,
    AttrFrozenRequiresExportedNonOpaqueData,
    AttrHotColdRequiresCallable,
    AttrHotColdConflict,
    AttrDeprecatedRequiresStringValue,
    AttrSinceRequiresVersionString,
    AttrOpaqueRequiresStructuralExport,
    AttrForeignRequiresForeignLet,
    AttrLinkRequiresStringValue,
    AttrWhenRequiresStringValue,
    AttrWhenRequiresStringList,
    ForeignSignatureRequired,
    InvalidPartialModifier,
    PartialForeignConflict,
    InvalidFfiType,
    LawMustBePure,
    CollectDuplicateDataVariant,
    DuplicateDataVariantDiscriminant,
    InvalidDataVariantDiscriminant,
    CyclicDataVariantDiscriminant,
    RuntimeValueInComptimeContext,
    CollectDuplicateEffectOp,
    CollectDuplicateEffectLaw,
    CollectDuplicateShapeMember,
    CollectDuplicateShapeLaw,
    UnknownExport,
    InvalidRequestTarget,
    UnknownEffect,
    DuplicateHandlerClause,
    UnknownEffectOp,
    HandlerClauseArityMismatch,
    HandleRequiresSingleValueClause,
    HandlerMissingOperationClause,
    ResumeOutsideHandlerClause,
    EffectNotDeclared,
    GivenMemberArityMismatch,
    UnknownGivenMember,
    GivenMemberValueRequired,
    DuplicateGivenMember,
    MissingGivenMember,
    InvalidGivenTarget,
    SealedShape,
    UnknownShape,
    DuplicateGiven,
    PlainLetRequiresIrrefutablePattern,
    ModuleDestructuringRequiresStaticModule,
    RecordDestructuringRequiresRecordOrModule,
    CallableLetRequiresSimpleBindingPattern,
    ArraySpreadRequiresOneDimensionalArray,
    InvalidSpreadSource,
    DuplicateRecordField,
    MissingRecordField,
    VariantMissingDataContext,
    VariantConstructorArityMismatch,
    VariantNamedFieldsRequired,
    VariantNamedFieldsUnexpected,
    DuplicateVariantField,
    MissingVariantField,
    UnknownVariantField,
    MixedVariantPayloadStyle,
    InvalidVariantArity,
    UnknownDataVariant,
    RecordLiteralRequiresNamedFields,
    ArrayLiteralLengthUnknownFromRuntimeSpread,
    ArrayLiteralLengthMismatch,
    SumConstructorArityMismatch,
    InvalidIndexArgCount,
    InvalidCallTarget,
    CallArityMismatch,
    CallPositionalAfterNamedArgument,
    CallSpreadAfterNamedArgument,
    CallNamedArgumentUnknown,
    CallNamedArgumentDuplicate,
    CallNamedArgumentAlreadyProvided,
    CallNamedArgumentsAfterRuntimeSpread,
    CallNamedSpreadArgument,
    UnsafeCallRequiresUnsafeBlock,
    UnsupportedPinTarget,
    PinnedValueEscapes,
    InvalidTypeApplication,
    CallRuntimeSpreadRequiresArrayAny,
    CallSpreadRequiresTupleOrArray,
    DeclarationUsedAsValue,
    TargetGateRejected,
    InvalidIndexTarget,
    UnknownField,
    AmbiguousDotCallable,
    DotCallableRequiresMutableReceiver,
    InvalidFieldTarget,
    InvalidRecordUpdateTarget,
    MutForbiddenInTypeTestTarget,
    MutForbiddenInTypeCastTarget,
    WriteTargetRequiresMut,
    UnsupportedAssignmentTarget,
    NumericOperandRequired,
    BinaryOperatorHasNoExecutableLowering,
    TypeMismatch,
    InvalidTypeExpression,
    TypeApplicationArityMismatch,
    ArrayTypeRequiresItem,
    AmbiguousVariantTag,
    VariantPatternArityMismatch,
    OrPatternBindersMismatch,
    UnsatisfiedConstraint,
    AmbiguousGivenMatch,
    ConstrainedNonCallableBinding,
    ExportedCallableRequiresConcreteConstraints,
}

impl SemaDiagKind {
    #[must_use]
    pub fn code(self) -> DiagCode {
        DiagCode::new(diag_catalog_gen::code(self))
    }

    #[must_use]
    pub fn message(self) -> &'static str {
        diag_catalog_gen::message(self)
    }

    #[must_use]
    pub fn label(self) -> &'static str {
        diag_catalog_gen::primary(self)
    }

    #[must_use]
    pub fn secondary(self) -> Option<&'static str> {
        diag_catalog_gen::secondary(self)
    }

    #[must_use]
    pub fn message_with(self, context: &DiagContext) -> String {
        diag_catalog_gen::render_message(self, context)
    }

    #[must_use]
    pub fn label_with(self, context: &DiagContext) -> String {
        diag_catalog_gen::render_primary(self, context)
    }

    #[must_use]
    pub fn secondary_with(self, context: &DiagContext) -> Option<String> {
        diag_catalog_gen::render_secondary(self, context)
    }

    #[must_use]
    pub fn hint(self) -> Option<&'static str> {
        diag_catalog_gen::help(self)
    }

    #[must_use]
    pub fn from_code(code: DiagCode) -> Option<Self> {
        diag_catalog_gen::from_code(code.raw())
    }

    #[must_use]
    pub fn from_diag(diag: &Diag) -> Option<Self> {
        diag.code().and_then(Self::from_code)
    }
}

impl DiagnosticKind for SemaDiagKind {
    fn code(self) -> DiagCode {
        self.code()
    }

    fn phase(self) -> &'static str {
        "sema"
    }

    fn level(self) -> DiagLevel {
        DiagLevel::Error
    }

    fn message(self) -> &'static str {
        self.message()
    }

    fn primary(self) -> &'static str {
        self.label()
    }

    fn help(self) -> Option<&'static str> {
        self.hint()
    }
}
