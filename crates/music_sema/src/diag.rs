use music_base::diag::{Diag, DiagCode};

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
    AttrMusiLangRequiresPlainBindLet,
    AttrMusiLangRequiresNameString,
    AttrMusiIntrinsicRequiresOpcodeString,
    AttrLinkRequiresForeignLet,
    AttrDataLayoutRequiresDataTarget,
    AttrForeignRequiresForeignLet,
    AttrLinkRequiresStringValue,
    AttrWhenRequiresStringValue,
    AttrWhenRequiresStringList,
    ForeignSignatureRequired,
    InvalidFfiType,
    LawMustBePure,
    CollectDuplicateDataVariant,
    CollectDuplicateEffectOp,
    CollectDuplicateEffectLaw,
    CollectDuplicateClassMember,
    CollectDuplicateClassLaw,
    UnknownExport,
    InvalidPerformTarget,
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
    InvalidArraySpreadSource,
    InvalidRecordSpreadSource,
    DuplicateRecordField,
    VariantMissingDataContext,
    VariantConstructorArityMismatch,
    InvalidVariantArity,
    UnknownDataVariant,
    RecordLiteralRequiresNamedFields,
    ArrayLiteralLengthUnknownFromRuntimeSpread,
    ArrayLiteralLengthMismatch,
    SumConstructorArityMismatch,
    InvalidIndexArity,
    InvalidCallTarget,
    CallArityMismatch,
    InvalidTypeApplication,
    InvalidCallSpreadSource,
    CallRuntimeSpreadRequiresArrayAny,
    CallSpreadRequiresTupleOrArray,
    DeclarationUsedAsValue,
    TargetGateRejected,
    InvalidIndexTarget,
    IndexRequiresArgument,
    UnknownField,
    InvalidFieldAccess,
    InvalidOptionalFieldAccess,
    InvalidRecordUpdateTarget,
    MutForbiddenInTypeTestTarget,
    MutForbiddenInTypeCastTarget,
    WriteRequiresMutValue,
    WriteRequiresMutArray,
    WriteRequiresMutRecord,
    InvalidFieldUpdateTarget,
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
}

impl SemaDiagKind {
    #[must_use]
    pub const fn code(self) -> DiagCode {
        DiagCode::new(match self {
            Self::AttrDuplicateRepr => 3000,
            Self::AttrReprRequiresKindString => 3001,
            Self::AttrLayoutArgRequiresName => 3002,
            Self::AttrDuplicateLayoutAlign => 3003,
            Self::AttrLayoutAlignRequiresU32 => 3004,
            Self::AttrDuplicateLayoutPack => 3005,
            Self::AttrLayoutPackRequiresU32 => 3006,
            Self::AttrUnknownArg => 3007,
            Self::AttrMusiLangRequiresPlainBindLet => 3008,
            Self::AttrMusiLangRequiresNameString => 3009,
            Self::AttrMusiIntrinsicRequiresOpcodeString => 3010,
            Self::AttrLinkRequiresForeignLet => 3011,
            Self::AttrDataLayoutRequiresDataTarget => 3012,
            Self::AttrForeignRequiresForeignLet => 3013,
            Self::AttrLinkRequiresStringValue => 3014,
            Self::AttrWhenRequiresStringValue => 3015,
            Self::AttrWhenRequiresStringList => 3016,
            Self::ForeignSignatureRequired => 3017,
            Self::InvalidFfiType => 3018,
            Self::LawMustBePure => 3019,
            Self::CollectDuplicateDataVariant => 3020,
            Self::CollectDuplicateEffectOp => 3021,
            Self::CollectDuplicateEffectLaw => 3022,
            Self::CollectDuplicateClassMember => 3023,
            Self::CollectDuplicateClassLaw => 3024,
            Self::UnknownExport => 3025,
            Self::InvalidPerformTarget => 3026,
            Self::UnknownEffect => 3027,
            Self::DuplicateHandlerClause => 3028,
            Self::UnknownEffectOp => 3029,
            Self::HandlerClauseArityMismatch => 3030,
            Self::HandleRequiresSingleValueClause => 3031,
            Self::HandlerMissingOperationClause => 3032,
            Self::ResumeOutsideHandlerClause => 3033,
            Self::EffectNotDeclared => 3034,
            Self::InstanceMemberArityMismatch => 3035,
            Self::UnknownInstanceMember => 3036,
            Self::InstanceMemberValueRequired => 3037,
            Self::DuplicateInstanceMember => 3038,
            Self::MissingInstanceMember => 3039,
            Self::InvalidInstanceTarget => 3040,
            Self::SealedClass => 3041,
            Self::UnknownClass => 3042,
            Self::DuplicateInstance => 3043,
            Self::PlainLetRequiresIrrefutablePattern => 3044,
            Self::ModuleDestructuringRequiresStaticModule => 3045,
            Self::RecordDestructuringRequiresRecordOrModule => 3046,
            Self::CallableLetRequiresSimpleBindingPattern => 3047,
            Self::ArraySpreadRequiresOneDimensionalArray => 3048,
            Self::InvalidArraySpreadSource => 3049,
            Self::InvalidRecordSpreadSource => 3050,
            Self::DuplicateRecordField => 3051,
            Self::VariantMissingDataContext => 3052,
            Self::VariantConstructorArityMismatch => 3053,
            Self::InvalidVariantArity => 3054,
            Self::UnknownDataVariant => 3055,
            Self::RecordLiteralRequiresNamedFields => 3056,
            Self::ArrayLiteralLengthUnknownFromRuntimeSpread => 3057,
            Self::ArrayLiteralLengthMismatch => 3058,
            Self::SumConstructorArityMismatch => 3059,
            Self::InvalidIndexArity => 3060,
            Self::InvalidCallTarget => 3061,
            Self::CallArityMismatch => 3062,
            Self::InvalidTypeApplication => 3063,
            Self::InvalidCallSpreadSource => 3064,
            Self::CallRuntimeSpreadRequiresArrayAny => 3065,
            Self::CallSpreadRequiresTupleOrArray => 3066,
            Self::DeclarationUsedAsValue => 3067,
            Self::TargetGateRejected => 3068,
            Self::InvalidIndexTarget => 3069,
            Self::IndexRequiresArgument => 3070,
            Self::UnknownField => 3071,
            Self::InvalidFieldAccess => 3072,
            Self::InvalidOptionalFieldAccess => 3073,
            Self::InvalidRecordUpdateTarget => 3074,
            Self::MutForbiddenInTypeTestTarget => 3075,
            Self::MutForbiddenInTypeCastTarget => 3076,
            Self::WriteRequiresMutValue => 3077,
            Self::WriteRequiresMutArray => 3078,
            Self::WriteRequiresMutRecord => 3079,
            Self::InvalidFieldUpdateTarget => 3080,
            Self::UnsupportedAssignmentTarget => 3081,
            Self::NumericOperandRequired => 3082,
            Self::BinaryOperatorHasNoExecutableLowering => 3083,
            Self::TypeMismatch => 3084,
            Self::InvalidTypeExpression => 3085,
            Self::TypeApplicationArityMismatch => 3086,
            Self::ArrayTypeRequiresItem => 3087,
            Self::AmbiguousVariantTag => 3088,
            Self::VariantPatternArityMismatch => 3089,
            Self::OrPatternBindersMismatch => 3090,
            Self::UnsatisfiedConstraint => 3091,
            Self::AmbiguousInstanceMatch => 3092,
        })
    }

    #[must_use]
    pub const fn message(self) -> &'static str {
        match self {
            Self::AttrDuplicateRepr
            | Self::AttrReprRequiresKindString
            | Self::AttrLayoutArgRequiresName
            | Self::AttrDuplicateLayoutAlign
            | Self::AttrLayoutAlignRequiresU32
            | Self::AttrDuplicateLayoutPack
            | Self::AttrLayoutPackRequiresU32
            | Self::AttrUnknownArg
            | Self::AttrMusiLangRequiresPlainBindLet
            | Self::AttrMusiLangRequiresNameString
            | Self::AttrMusiIntrinsicRequiresOpcodeString
            | Self::AttrLinkRequiresForeignLet
            | Self::AttrDataLayoutRequiresDataTarget
            | Self::AttrForeignRequiresForeignLet
            | Self::AttrLinkRequiresStringValue
            | Self::AttrWhenRequiresStringValue
            | Self::AttrWhenRequiresStringList
            | Self::ForeignSignatureRequired
            | Self::InvalidFfiType => attr_message(self),
            Self::CollectDuplicateDataVariant
            | Self::CollectDuplicateEffectOp
            | Self::CollectDuplicateEffectLaw
            | Self::CollectDuplicateClassMember
            | Self::CollectDuplicateClassLaw
            | Self::UnknownExport
            | Self::InvalidPerformTarget
            | Self::UnknownEffect
            | Self::DuplicateHandlerClause
            | Self::UnknownEffectOp
            | Self::HandlerClauseArityMismatch
            | Self::HandleRequiresSingleValueClause
            | Self::HandlerMissingOperationClause
            | Self::ResumeOutsideHandlerClause
            | Self::EffectNotDeclared
            | Self::InstanceMemberArityMismatch
            | Self::UnknownInstanceMember
            | Self::InstanceMemberValueRequired
            | Self::DuplicateInstanceMember
            | Self::MissingInstanceMember
            | Self::InvalidInstanceTarget
            | Self::SealedClass
            | Self::UnknownClass
            | Self::DuplicateInstance
            | Self::PlainLetRequiresIrrefutablePattern
            | Self::ModuleDestructuringRequiresStaticModule
            | Self::RecordDestructuringRequiresRecordOrModule
            | Self::CallableLetRequiresSimpleBindingPattern
            | Self::LawMustBePure => decl_message(self),
            _ => expr_message(self),
        }
    }

    #[must_use]
    pub const fn label(self) -> &'static str {
        self.message()
    }
}

const fn attr_message(kind: SemaDiagKind) -> &'static str {
    match kind {
        SemaDiagKind::AttrDuplicateRepr => "duplicate `@repr`",
        SemaDiagKind::AttrReprRequiresKindString => "`@repr` requires string `kind` arg",
        SemaDiagKind::AttrLayoutArgRequiresName => "`@layout` arg requires name",
        SemaDiagKind::AttrDuplicateLayoutAlign => "duplicate `@layout align`",
        SemaDiagKind::AttrLayoutAlignRequiresU32 => "`@layout align` requires u32 value",
        SemaDiagKind::AttrDuplicateLayoutPack => "duplicate `@layout pack`",
        SemaDiagKind::AttrLayoutPackRequiresU32 => "`@layout pack` requires u32 value",
        SemaDiagKind::AttrUnknownArg => "unknown attr arg",
        SemaDiagKind::AttrMusiLangRequiresPlainBindLet => {
            "`@musi.lang` requires plain bind `let` target"
        }
        SemaDiagKind::AttrMusiLangRequiresNameString => "`@musi.lang` requires string `name` arg",
        SemaDiagKind::AttrMusiIntrinsicRequiresOpcodeString => {
            "`@musi.intrinsic` requires string `opcode` arg"
        }
        SemaDiagKind::AttrLinkRequiresForeignLet => "`@link` requires foreign `let` target",
        SemaDiagKind::AttrDataLayoutRequiresDataTarget => "data layout attr requires data target",
        SemaDiagKind::AttrForeignRequiresForeignLet => "foreign attr requires foreign `let` target",
        SemaDiagKind::AttrLinkRequiresStringValue => "`@link` requires string values",
        SemaDiagKind::AttrWhenRequiresStringValue => "`@when` requires string values",
        SemaDiagKind::AttrWhenRequiresStringList => "`@when` requires string list for `feature`",
        SemaDiagKind::ForeignSignatureRequired => "foreign signature required",
        SemaDiagKind::InvalidFfiType => "invalid ffi type",
        _ => "",
    }
}

const fn decl_message(kind: SemaDiagKind) -> &'static str {
    match kind {
        SemaDiagKind::CollectDuplicateDataVariant => "duplicate data variant",
        SemaDiagKind::CollectDuplicateEffectOp => "duplicate effect op",
        SemaDiagKind::CollectDuplicateEffectLaw => "duplicate effect law",
        SemaDiagKind::CollectDuplicateClassMember => "duplicate class member",
        SemaDiagKind::CollectDuplicateClassLaw => "duplicate class law",
        SemaDiagKind::UnknownExport => "unknown export",
        SemaDiagKind::InvalidPerformTarget => "invalid perform target",
        SemaDiagKind::UnknownEffect => "unknown effect",
        SemaDiagKind::DuplicateHandlerClause => "duplicate handler clause",
        SemaDiagKind::UnknownEffectOp => "unknown effect op",
        SemaDiagKind::HandlerClauseArityMismatch => "handler clause arity mismatch",
        SemaDiagKind::HandleRequiresSingleValueClause => "handle requires exactly one value clause",
        SemaDiagKind::HandlerMissingOperationClause => "handler missing operation clause",
        SemaDiagKind::ResumeOutsideHandlerClause => "resume outside handler clause",
        SemaDiagKind::EffectNotDeclared => "effect not declared",
        SemaDiagKind::InstanceMemberArityMismatch => "instance member arity mismatch",
        SemaDiagKind::UnknownInstanceMember => "unknown instance member",
        SemaDiagKind::InstanceMemberValueRequired => "instance member value required",
        SemaDiagKind::DuplicateInstanceMember => "duplicate instance member",
        SemaDiagKind::MissingInstanceMember => "missing instance member",
        SemaDiagKind::InvalidInstanceTarget => "invalid instance target",
        SemaDiagKind::SealedClass => "sealed class",
        SemaDiagKind::UnknownClass => "unknown class",
        SemaDiagKind::DuplicateInstance => "duplicate instance",
        SemaDiagKind::PlainLetRequiresIrrefutablePattern => {
            "plain `let` requires irrefutable pattern"
        }
        SemaDiagKind::ModuleDestructuringRequiresStaticModule => {
            "module destructuring requires static module value"
        }
        SemaDiagKind::RecordDestructuringRequiresRecordOrModule => {
            "record destructuring requires record or module value"
        }
        SemaDiagKind::CallableLetRequiresSimpleBindingPattern => {
            "callable `let` requires simple binding pattern"
        }
        SemaDiagKind::LawMustBePure => "law must be pure",
        _ => "",
    }
}

const fn expr_message(kind: SemaDiagKind) -> &'static str {
    match kind {
        SemaDiagKind::ArraySpreadRequiresOneDimensionalArray => {
            "array spread requires one-dimensional array"
        }
        SemaDiagKind::InvalidArraySpreadSource => "invalid array spread source",
        SemaDiagKind::InvalidRecordSpreadSource => "invalid record spread source",
        SemaDiagKind::DuplicateRecordField => "duplicate record field",
        SemaDiagKind::VariantMissingDataContext => "variant constructor requires data type context",
        SemaDiagKind::VariantConstructorArityMismatch => "variant constructor arity mismatch",
        SemaDiagKind::InvalidVariantArity => "invalid variant arity",
        SemaDiagKind::UnknownDataVariant => "unknown data variant",
        SemaDiagKind::RecordLiteralRequiresNamedFields => "record literal requires named fields",
        SemaDiagKind::ArrayLiteralLengthUnknownFromRuntimeSpread => {
            "array literal length unknown from runtime spread"
        }
        SemaDiagKind::ArrayLiteralLengthMismatch => "array literal length mismatch",
        SemaDiagKind::SumConstructorArityMismatch => "sum constructor arity mismatch",
        SemaDiagKind::InvalidIndexArity => "invalid index arity",
        SemaDiagKind::InvalidCallTarget => "invalid call target",
        SemaDiagKind::CallArityMismatch => "call arity mismatch",
        SemaDiagKind::InvalidTypeApplication => "invalid type application",
        SemaDiagKind::InvalidCallSpreadSource => "invalid call spread source",
        SemaDiagKind::CallRuntimeSpreadRequiresArrayAny => {
            "call runtime spread requires `Array[Any]`"
        }
        SemaDiagKind::CallSpreadRequiresTupleOrArray => "call spread requires tuple or array",
        SemaDiagKind::DeclarationUsedAsValue => "declaration form used as value",
        SemaDiagKind::TargetGateRejected => "target gate rejected item",
        SemaDiagKind::InvalidIndexTarget => "invalid index target",
        SemaDiagKind::IndexRequiresArgument => "index requires one or more args",
        SemaDiagKind::UnknownField => "unknown field",
        SemaDiagKind::InvalidFieldAccess => "invalid field access",
        SemaDiagKind::InvalidOptionalFieldAccess => "invalid optional field access",
        SemaDiagKind::InvalidRecordUpdateTarget => "invalid record update target",
        SemaDiagKind::MutForbiddenInTypeTestTarget => "`mut` not allowed in type test target",
        SemaDiagKind::MutForbiddenInTypeCastTarget => "`mut` not allowed in type cast target",
        SemaDiagKind::WriteRequiresMutValue => "write requires `mut T`",
        SemaDiagKind::WriteRequiresMutArray => "write requires `mut []T`",
        SemaDiagKind::WriteRequiresMutRecord => "write requires `mut { ... }`",
        SemaDiagKind::InvalidFieldUpdateTarget => "invalid field update target",
        SemaDiagKind::UnsupportedAssignmentTarget => "unsupported assignment target",
        SemaDiagKind::NumericOperandRequired => "numeric operand required",
        SemaDiagKind::BinaryOperatorHasNoExecutableLowering => {
            "binary operator has no executable lowering"
        }
        SemaDiagKind::TypeMismatch => "type mismatch",
        SemaDiagKind::InvalidTypeExpression => "invalid type expression",
        SemaDiagKind::TypeApplicationArityMismatch => "type application arity mismatch",
        SemaDiagKind::ArrayTypeRequiresItem => "Array requires at least one arg",
        SemaDiagKind::AmbiguousVariantTag => "ambiguous variant tag; add type annotation",
        SemaDiagKind::VariantPatternArityMismatch => "variant pattern arity mismatch",
        SemaDiagKind::OrPatternBindersMismatch => "or-pattern binders must match",
        SemaDiagKind::UnsatisfiedConstraint => "unsatisfied constraint",
        SemaDiagKind::AmbiguousInstanceMatch => "ambiguous instance match",
        _ => "",
    }
}

impl SemaDiagKind {
    #[must_use]
    pub const fn from_code(code: DiagCode) -> Option<Self> {
        Some(match code.raw() {
            3000 => Self::AttrDuplicateRepr,
            3001 => Self::AttrReprRequiresKindString,
            3002 => Self::AttrLayoutArgRequiresName,
            3003 => Self::AttrDuplicateLayoutAlign,
            3004 => Self::AttrLayoutAlignRequiresU32,
            3005 => Self::AttrDuplicateLayoutPack,
            3006 => Self::AttrLayoutPackRequiresU32,
            3007 => Self::AttrUnknownArg,
            3008 => Self::AttrMusiLangRequiresPlainBindLet,
            3009 => Self::AttrMusiLangRequiresNameString,
            3010 => Self::AttrMusiIntrinsicRequiresOpcodeString,
            3011 => Self::AttrLinkRequiresForeignLet,
            3012 => Self::AttrDataLayoutRequiresDataTarget,
            3013 => Self::AttrForeignRequiresForeignLet,
            3014 => Self::AttrLinkRequiresStringValue,
            3015 => Self::AttrWhenRequiresStringValue,
            3016 => Self::AttrWhenRequiresStringList,
            3017 => Self::ForeignSignatureRequired,
            3018 => Self::InvalidFfiType,
            3019 => Self::LawMustBePure,
            3020 => Self::CollectDuplicateDataVariant,
            3021 => Self::CollectDuplicateEffectOp,
            3022 => Self::CollectDuplicateEffectLaw,
            3023 => Self::CollectDuplicateClassMember,
            3024 => Self::CollectDuplicateClassLaw,
            3025 => Self::UnknownExport,
            3026 => Self::InvalidPerformTarget,
            3027 => Self::UnknownEffect,
            3028 => Self::DuplicateHandlerClause,
            3029 => Self::UnknownEffectOp,
            3030 => Self::HandlerClauseArityMismatch,
            3031 => Self::HandleRequiresSingleValueClause,
            3032 => Self::HandlerMissingOperationClause,
            3033 => Self::ResumeOutsideHandlerClause,
            3034 => Self::EffectNotDeclared,
            3035 => Self::InstanceMemberArityMismatch,
            3036 => Self::UnknownInstanceMember,
            3037 => Self::InstanceMemberValueRequired,
            3038 => Self::DuplicateInstanceMember,
            3039 => Self::MissingInstanceMember,
            3040 => Self::InvalidInstanceTarget,
            3041 => Self::SealedClass,
            3042 => Self::UnknownClass,
            3043 => Self::DuplicateInstance,
            3044 => Self::PlainLetRequiresIrrefutablePattern,
            3045 => Self::ModuleDestructuringRequiresStaticModule,
            3046 => Self::RecordDestructuringRequiresRecordOrModule,
            3047 => Self::CallableLetRequiresSimpleBindingPattern,
            3048 => Self::ArraySpreadRequiresOneDimensionalArray,
            3049 => Self::InvalidArraySpreadSource,
            3050 => Self::InvalidRecordSpreadSource,
            3051 => Self::DuplicateRecordField,
            3052 => Self::VariantMissingDataContext,
            3053 => Self::VariantConstructorArityMismatch,
            3054 => Self::InvalidVariantArity,
            3055 => Self::UnknownDataVariant,
            3056 => Self::RecordLiteralRequiresNamedFields,
            3057 => Self::ArrayLiteralLengthUnknownFromRuntimeSpread,
            3058 => Self::ArrayLiteralLengthMismatch,
            3059 => Self::SumConstructorArityMismatch,
            3060 => Self::InvalidIndexArity,
            3061 => Self::InvalidCallTarget,
            3062 => Self::CallArityMismatch,
            3063 => Self::InvalidTypeApplication,
            3064 => Self::InvalidCallSpreadSource,
            3065 => Self::CallRuntimeSpreadRequiresArrayAny,
            3066 => Self::CallSpreadRequiresTupleOrArray,
            3067 => Self::DeclarationUsedAsValue,
            3068 => Self::TargetGateRejected,
            3069 => Self::InvalidIndexTarget,
            3070 => Self::IndexRequiresArgument,
            3071 => Self::UnknownField,
            3072 => Self::InvalidFieldAccess,
            3073 => Self::InvalidOptionalFieldAccess,
            3074 => Self::InvalidRecordUpdateTarget,
            3075 => Self::MutForbiddenInTypeTestTarget,
            3076 => Self::MutForbiddenInTypeCastTarget,
            3077 => Self::WriteRequiresMutValue,
            3078 => Self::WriteRequiresMutArray,
            3079 => Self::WriteRequiresMutRecord,
            3080 => Self::InvalidFieldUpdateTarget,
            3081 => Self::UnsupportedAssignmentTarget,
            3082 => Self::NumericOperandRequired,
            3083 => Self::BinaryOperatorHasNoExecutableLowering,
            3084 => Self::TypeMismatch,
            3085 => Self::InvalidTypeExpression,
            3086 => Self::TypeApplicationArityMismatch,
            3087 => Self::ArrayTypeRequiresItem,
            3088 => Self::AmbiguousVariantTag,
            3089 => Self::VariantPatternArityMismatch,
            3090 => Self::OrPatternBindersMismatch,
            3091 => Self::UnsatisfiedConstraint,
            3092 => Self::AmbiguousInstanceMatch,
            _ => return None,
        })
    }

    #[must_use]
    pub fn from_diag(diag: &Diag) -> Option<Self> {
        diag.code().and_then(Self::from_code)
    }
}
