use music_base::diag::{Diag, DiagCode};

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
    CollectDuplicateClassMember,
    CollectDuplicateClassLaw,
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
    InstanceMemberArityMismatch,
    UnknownInstanceMember,
    InstanceMemberValueRequired,
    DuplicateInstanceMember,
    MissingInstanceMember,
    InvalidInstanceTarget,
    SealedClass,
    UnknownClass,
    DuplicateInstance,
    PlainLetRequiresIrrefutablePattern,
    ModuleDestructuringRequiresStaticModule,
    RecordDestructuringRequiresRecordOrModule,
    CallableLetRequiresSimpleBindingPattern,
    ArraySpreadRequiresOneDimensionalArray,
    InvalidSpreadSource,
    DuplicateRecordField,
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
    InvalidTypeApplication,
    CallRuntimeSpreadRequiresArrayAny,
    CallSpreadRequiresTupleOrArray,
    DeclarationUsedAsValue,
    TargetGateRejected,
    InvalidIndexTarget,
    UnknownField,
    AmbiguousAttachedMethod,
    AttachedMethodRequiresMutReceiver,
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
    AmbiguousInstanceMatch,
    ConstrainedNonCallableBinding,
    ExportedCallableRequiresConcreteConstraints,
}

impl SemaDiagKind {
    #[must_use]
    pub fn code(self) -> DiagCode {
        DiagCode::new(self.info().code)
    }

    #[must_use]
    pub fn message(self) -> &'static str {
        self.info().message
    }

    #[must_use]
    pub fn label(self) -> &'static str {
        for label in SEMA_DIAG_LABELS {
            if label.kind == self {
                return label.text;
            }
        }
        self.message()
    }

    #[must_use]
    pub fn hint(self) -> Option<&'static str> {
        for hint in SEMA_DIAG_HINTS {
            if hint.kind == self {
                return Some(hint.text);
            }
        }
        None
    }

    fn info(self) -> &'static SemaDiagInfo {
        let mut index = 0;
        while index < SEMA_DIAG_INFOS.len() {
            let info = &SEMA_DIAG_INFOS[index];
            if info.kind == self {
                return info;
            }
            index += 1;
        }
        &SEMA_DIAG_INFOS[0]
    }

    #[must_use]
    pub fn from_code(code: DiagCode) -> Option<Self> {
        let mut index = 0;
        while index < SEMA_DIAG_INFOS.len() {
            let info = &SEMA_DIAG_INFOS[index];
            if info.code == code.raw() {
                return Some(info.kind);
            }
            index += 1;
        }
        None
    }

    #[must_use]
    pub fn from_diag(diag: &Diag) -> Option<Self> {
        diag.code().and_then(Self::from_code)
    }
}

struct SemaDiagInfo {
    kind: SemaDiagKind,
    code: u16,
    message: &'static str,
}

struct SemaDiagText {
    kind: SemaDiagKind,
    text: &'static str,
}

const SEMA_DIAG_LABELS: &[SemaDiagText] = &[
    SemaDiagText {
        kind: SemaDiagKind::InvalidRequestTarget,
        text: "request target must be effect operation call",
    },
    SemaDiagText {
        kind: SemaDiagKind::DuplicateHandlerClause,
        text: "duplicate handler clause here",
    },
    SemaDiagText {
        kind: SemaDiagKind::HandlerClauseArityMismatch,
        text: "handler clause parameters do not match operation",
    },
    SemaDiagText {
        kind: SemaDiagKind::HandleRequiresSingleValueClause,
        text: "handler literal requires exactly one `value` clause",
    },
    SemaDiagText {
        kind: SemaDiagKind::HandlerMissingOperationClause,
        text: "handler literal is missing handled operation clause",
    },
    SemaDiagText {
        kind: SemaDiagKind::ResumeOutsideHandlerClause,
        text: "`resume` is only valid inside handler operation clause",
    },
    SemaDiagText {
        kind: SemaDiagKind::EffectNotDeclared,
        text: "effect must appear in surrounding `using` set",
    },
    SemaDiagText {
        kind: SemaDiagKind::InvalidSpreadSource,
        text: "spread source is not valid here",
    },
    SemaDiagText {
        kind: SemaDiagKind::InvalidIndexArgCount,
        text: "index expression has invalid argument count",
    },
    SemaDiagText {
        kind: SemaDiagKind::InvalidIndexTarget,
        text: "index target must support indexing",
    },
    SemaDiagText {
        kind: SemaDiagKind::InvalidCallTarget,
        text: "callee must be callable",
    },
    SemaDiagText {
        kind: SemaDiagKind::CallArityMismatch,
        text: "call argument count does not match callee",
    },
    SemaDiagText {
        kind: SemaDiagKind::CallPositionalAfterNamedArgument,
        text: "positional call argument cannot follow named argument",
    },
    SemaDiagText {
        kind: SemaDiagKind::CallSpreadAfterNamedArgument,
        text: "call spread cannot follow named argument",
    },
    SemaDiagText {
        kind: SemaDiagKind::CallNamedArgumentUnknown,
        text: "named call argument not declared here",
    },
    SemaDiagText {
        kind: SemaDiagKind::CallNamedArgumentDuplicate,
        text: "named call argument appears more than once",
    },
    SemaDiagText {
        kind: SemaDiagKind::CallNamedArgumentAlreadyProvided,
        text: "named call argument is already provided",
    },
    SemaDiagText {
        kind: SemaDiagKind::CallNamedArgumentsAfterRuntimeSpread,
        text: "named call arguments cannot follow runtime spread",
    },
    SemaDiagText {
        kind: SemaDiagKind::CallNamedSpreadArgument,
        text: "call spread cannot be named",
    },
    SemaDiagText {
        kind: SemaDiagKind::UnsafeCallRequiresUnsafeBlock,
        text: "call must appear inside `unsafe` block",
    },
    SemaDiagText {
        kind: SemaDiagKind::CallRuntimeSpreadRequiresArrayAny,
        text: "runtime call spread requires `[]Any`",
    },
    SemaDiagText {
        kind: SemaDiagKind::CallSpreadRequiresTupleOrArray,
        text: "call spread requires tuple or one-dimensional array",
    },
    SemaDiagText {
        kind: SemaDiagKind::InvalidFieldTarget,
        text: "field target is not valid here",
    },
    SemaDiagText {
        kind: SemaDiagKind::InvalidRecordUpdateTarget,
        text: "record update target must be record value",
    },
    SemaDiagText {
        kind: SemaDiagKind::WriteTargetRequiresMut,
        text: "assignment target must be mutable",
    },
    SemaDiagText {
        kind: SemaDiagKind::UnsupportedAssignmentTarget,
        text: "assignment target is not writable",
    },
    SemaDiagText {
        kind: SemaDiagKind::AmbiguousVariantTag,
        text: "variant tag needs type context",
    },
    SemaDiagText {
        kind: SemaDiagKind::VariantNamedFieldsRequired,
        text: "variant requires named payload fields",
    },
    SemaDiagText {
        kind: SemaDiagKind::VariantNamedFieldsUnexpected,
        text: "variant does not accept named payload fields",
    },
    SemaDiagText {
        kind: SemaDiagKind::DuplicateVariantField,
        text: "variant field appears more than once",
    },
    SemaDiagText {
        kind: SemaDiagKind::MissingVariantField,
        text: "variant field is missing",
    },
    SemaDiagText {
        kind: SemaDiagKind::UnknownVariantField,
        text: "variant field is not declared",
    },
    SemaDiagText {
        kind: SemaDiagKind::MixedVariantPayloadStyle,
        text: "variant payload fields must be all named or all positional",
    },
];

const SEMA_DIAG_HINTS: &[SemaDiagText] = &[
    SemaDiagText {
        kind: SemaDiagKind::InvalidRequestTarget,
        text: "write `request Effect.op(...)`",
    },
    SemaDiagText {
        kind: SemaDiagKind::HandleRequiresSingleValueClause,
        text: "add exactly one `value => ...` clause",
    },
    SemaDiagText {
        kind: SemaDiagKind::HandlerMissingOperationClause,
        text: "add clause for each handled operation",
    },
    SemaDiagText {
        kind: SemaDiagKind::ResumeOutsideHandlerClause,
        text: "move `resume` into handler operation clause",
    },
    SemaDiagText {
        kind: SemaDiagKind::EffectNotDeclared,
        text: "declare effect in `using` clause or remove request",
    },
    SemaDiagText {
        kind: SemaDiagKind::CallRuntimeSpreadRequiresArrayAny,
        text: "use `[]Any` for runtime-sized spread",
    },
    SemaDiagText {
        kind: SemaDiagKind::CallSpreadRequiresTupleOrArray,
        text: "spread tuple or one-dimensional array instead",
    },
    SemaDiagText {
        kind: SemaDiagKind::CallPositionalAfterNamedArgument,
        text: "move positional arguments before named arguments",
    },
    SemaDiagText {
        kind: SemaDiagKind::CallSpreadAfterNamedArgument,
        text: "move spread arguments before named arguments",
    },
    SemaDiagText {
        kind: SemaDiagKind::AmbiguousVariantTag,
        text: "add type annotation",
    },
];

const SEMA_DIAG_INFOS: &[SemaDiagInfo] = &[
    SemaDiagInfo {
        kind: SemaDiagKind::AttrDuplicateRepr,
        code: 3000,
        message: "duplicate `@repr`",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrReprRequiresKindString,
        code: 3001,
        message: "`@repr` requires string `kind` argument",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrLayoutArgRequiresName,
        code: 3002,
        message: "`@layout` argument requires name",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrDuplicateLayoutAlign,
        code: 3003,
        message: "duplicate `@layout align`",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrLayoutAlignRequiresU32,
        code: 3004,
        message: "`@layout align` requires u32 value",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrDuplicateLayoutPack,
        code: 3005,
        message: "duplicate `@layout pack`",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrLayoutPackRequiresU32,
        code: 3006,
        message: "`@layout pack` requires u32 value",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrUnknownArg,
        code: 3007,
        message: "unknown attribute argument",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrKnownRequiresPlainBindLet,
        code: 3008,
        message: "`@known` requires plain bind `let` target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrKnownRequiresNameString,
        code: 3009,
        message: "`@known` requires string `name` argument",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrKnownRequiresFoundationModule,
        code: 3010,
        message: "`@known` requires `musi:*` module",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrKnownRequiresExport,
        code: 3011,
        message: "`@known` requires exported binding",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrKnownUnknownName,
        code: 3012,
        message: "`@known` name unknown",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrIntrinsicRequiresNameString,
        code: 3013,
        message: "`@intrinsic` requires string `name` argument",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrIntrinsicRequiresIntrinsicsModule,
        code: 3014,
        message: "`@intrinsic` requires `musi:intrinsics` module",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrIntrinsicRequiresForeignLet,
        code: 3300,
        message: "`@intrinsic` requires foreign `let` target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrLinkRequiresForeignLet,
        code: 3015,
        message: "`@link` requires foreign `let` target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrDataLayoutRequiresDataTarget,
        code: 3016,
        message: "data layout attribute requires data target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrFrozenRequiresExportedNonOpaqueData,
        code: 3301,
        message: "`@frozen` requires exported non-opaque `data` target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrHotColdRequiresCallable,
        code: 3302,
        message: "`@hot` and `@cold` require callable target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrHotColdConflict,
        code: 3303,
        message: "`@hot` conflicts with `@cold`",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrDeprecatedRequiresStringValue,
        code: 3304,
        message: "`@deprecated` requires string values",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrSinceRequiresVersionString,
        code: 3305,
        message: "`@since` requires string `version` argument",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrOpaqueRequiresStructuralExport,
        code: 3306,
        message: "`opaque` requires exported structural target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrForeignRequiresForeignLet,
        code: 3017,
        message: "foreign attribute requires foreign `let` target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrLinkRequiresStringValue,
        code: 3018,
        message: "`@link` requires string values",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrWhenRequiresStringValue,
        code: 3019,
        message: "`@when` requires string values",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrWhenRequiresStringList,
        code: 3020,
        message: "`@when` requires string list for `feature`",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ForeignSignatureRequired,
        code: 3021,
        message: "foreign signature required",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidPartialModifier,
        code: 3120,
        message: "invalid `partial` target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::PartialForeignConflict,
        code: 3121,
        message: "`partial` conflicts with foreign binding",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidFfiType,
        code: 3022,
        message: "invalid foreign function interface type",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::LawMustBePure,
        code: 3023,
        message: "law must be pure",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CollectDuplicateDataVariant,
        code: 3024,
        message: "duplicate data variant",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::DuplicateDataVariantDiscriminant,
        code: 3025,
        message: "duplicate data variant discriminant",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidDataVariantDiscriminant,
        code: 3026,
        message: "invalid data variant discriminant",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CyclicDataVariantDiscriminant,
        code: 3027,
        message: "cyclic data variant discriminant",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CollectDuplicateEffectOp,
        code: 3028,
        message: "duplicate effect operation",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CollectDuplicateEffectLaw,
        code: 3029,
        message: "duplicate effect law",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CollectDuplicateClassMember,
        code: 3030,
        message: "duplicate class member",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CollectDuplicateClassLaw,
        code: 3031,
        message: "duplicate class law",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnknownExport,
        code: 3032,
        message: "unknown export",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidRequestTarget,
        code: 3033,
        message: "invalid request target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnknownEffect,
        code: 3034,
        message: "unknown effect",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::DuplicateHandlerClause,
        code: 3035,
        message: "duplicate handler clause",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnknownEffectOp,
        code: 3036,
        message: "unknown effect operation",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::HandlerClauseArityMismatch,
        code: 3037,
        message: "handler clause arity mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::HandleRequiresSingleValueClause,
        code: 3038,
        message: "handle requires exactly one value clause",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::HandlerMissingOperationClause,
        code: 3039,
        message: "handler missing operation clause",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ResumeOutsideHandlerClause,
        code: 3040,
        message: "resume outside handler clause",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::EffectNotDeclared,
        code: 3041,
        message: "effect not declared",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InstanceMemberArityMismatch,
        code: 3042,
        message: "instance member arity mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnknownInstanceMember,
        code: 3043,
        message: "unknown instance member",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InstanceMemberValueRequired,
        code: 3044,
        message: "instance member value required",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::DuplicateInstanceMember,
        code: 3045,
        message: "duplicate instance member",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::MissingInstanceMember,
        code: 3046,
        message: "missing instance member",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidInstanceTarget,
        code: 3047,
        message: "invalid instance target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::SealedClass,
        code: 3048,
        message: "sealed class",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnknownClass,
        code: 3049,
        message: "unknown class",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::DuplicateInstance,
        code: 3050,
        message: "duplicate instance",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::PlainLetRequiresIrrefutablePattern,
        code: 3051,
        message: "plain `let` requires irrefutable pattern",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ModuleDestructuringRequiresStaticModule,
        code: 3052,
        message: "module destructuring requires static module value",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::RecordDestructuringRequiresRecordOrModule,
        code: 3053,
        message: "record destructuring requires record or module value",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CallableLetRequiresSimpleBindingPattern,
        code: 3054,
        message: "callable `let` requires simple binding pattern",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ArraySpreadRequiresOneDimensionalArray,
        code: 3055,
        message: "array spread requires one-dimensional array",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidSpreadSource,
        code: 3056,
        message: "invalid spread source",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::DuplicateRecordField,
        code: 3058,
        message: "duplicate record field",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::VariantMissingDataContext,
        code: 3059,
        message: "variant constructor requires data type context",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::VariantConstructorArityMismatch,
        code: 3060,
        message: "variant constructor arity mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::VariantNamedFieldsRequired,
        code: 3105,
        message: "variant named fields required",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::VariantNamedFieldsUnexpected,
        code: 3106,
        message: "variant named fields not allowed",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::DuplicateVariantField,
        code: 3107,
        message: "duplicate variant field",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::MissingVariantField,
        code: 3108,
        message: "missing variant field",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnknownVariantField,
        code: 3109,
        message: "unknown variant field",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::MixedVariantPayloadStyle,
        code: 3110,
        message: "mixed variant payload style",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidVariantArity,
        code: 3061,
        message: "invalid variant arity",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnknownDataVariant,
        code: 3062,
        message: "unknown data variant",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::RecordLiteralRequiresNamedFields,
        code: 3063,
        message: "record literal requires named fields",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ArrayLiteralLengthUnknownFromRuntimeSpread,
        code: 3064,
        message: "array literal length unknown from runtime spread",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ArrayLiteralLengthMismatch,
        code: 3065,
        message: "array literal length mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::SumConstructorArityMismatch,
        code: 3066,
        message: "sum constructor arity mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidIndexArgCount,
        code: 3067,
        message: "invalid index argument count",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidCallTarget,
        code: 3068,
        message: "invalid call target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CallArityMismatch,
        code: 3069,
        message: "call arity mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CallPositionalAfterNamedArgument,
        code: 3111,
        message: "positional call argument after named argument",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CallSpreadAfterNamedArgument,
        code: 3112,
        message: "call spread after named argument",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CallNamedArgumentUnknown,
        code: 3113,
        message: "unknown named call argument",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CallNamedArgumentDuplicate,
        code: 3114,
        message: "duplicate named call argument",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CallNamedArgumentAlreadyProvided,
        code: 3115,
        message: "named call argument already provided",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CallNamedArgumentsAfterRuntimeSpread,
        code: 3116,
        message: "named call arguments after runtime spread",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CallNamedSpreadArgument,
        code: 3117,
        message: "named spread call argument",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnsafeCallRequiresUnsafeBlock,
        code: 3118,
        message: "unsafe block required for call",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidTypeApplication,
        code: 3070,
        message: "invalid type application",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CallRuntimeSpreadRequiresArrayAny,
        code: 3072,
        message: "call runtime spread requires `[]Any`",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CallSpreadRequiresTupleOrArray,
        code: 3073,
        message: "call spread requires tuple or array",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::DeclarationUsedAsValue,
        code: 3074,
        message: "declaration form used as value",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::TargetGateRejected,
        code: 3075,
        message: "target gate rejected item",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidIndexTarget,
        code: 3076,
        message: "invalid index target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnknownField,
        code: 3078,
        message: "unknown field",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AmbiguousAttachedMethod,
        code: 3102,
        message: "ambiguous attached method match",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttachedMethodRequiresMutReceiver,
        code: 3103,
        message: "attached method requires mutable receiver",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidFieldTarget,
        code: 3079,
        message: "invalid field target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidRecordUpdateTarget,
        code: 3081,
        message: "invalid record update target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::MutForbiddenInTypeTestTarget,
        code: 3082,
        message: "`mut` not allowed in type test target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::MutForbiddenInTypeCastTarget,
        code: 3083,
        message: "`mut` not allowed in type cast target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::WriteTargetRequiresMut,
        code: 3084,
        message: "write target requires `mut`",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnsupportedAssignmentTarget,
        code: 3088,
        message: "unsupported assignment target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::NumericOperandRequired,
        code: 3089,
        message: "numeric operand required",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::BinaryOperatorHasNoExecutableLowering,
        code: 3090,
        message: "binary operator has no executable lowering",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::TypeMismatch,
        code: 3091,
        message: "type mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidTypeExpression,
        code: 3092,
        message: "invalid type expression",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::TypeApplicationArityMismatch,
        code: 3093,
        message: "type application arity mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ArrayTypeRequiresItem,
        code: 3094,
        message: "array type requires item type",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AmbiguousVariantTag,
        code: 3095,
        message: "ambiguous variant tag; add type annotation",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::VariantPatternArityMismatch,
        code: 3096,
        message: "variant pattern arity mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::OrPatternBindersMismatch,
        code: 3097,
        message: "or-pattern binders must match",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnsatisfiedConstraint,
        code: 3098,
        message: "unsatisfied constraint",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AmbiguousInstanceMatch,
        code: 3099,
        message: "ambiguous instance match",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ConstrainedNonCallableBinding,
        code: 3100,
        message: "non-callable `let` cannot have `where` constraints",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ExportedCallableRequiresConcreteConstraints,
        code: 3101,
        message: "exported callable requires fully resolved constraints",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::RuntimeValueInComptimeContext,
        code: 3122,
        message: "runtime value in `comptime` context",
    },
];
