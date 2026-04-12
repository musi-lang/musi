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
        self.message()
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

const SEMA_DIAG_INFOS: &[SemaDiagInfo] = &[
    SemaDiagInfo {
        kind: SemaDiagKind::AttrDuplicateRepr,
        code: 3000,
        message: "duplicate `@repr`",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrReprRequiresKindString,
        code: 3001,
        message: "`@repr` requires string `kind` arg",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrLayoutArgRequiresName,
        code: 3002,
        message: "`@layout` arg requires name",
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
        message: "unknown attr arg",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrMusiLangRequiresPlainBindLet,
        code: 3008,
        message: "`@musi.lang` requires plain bind `let` target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrMusiLangRequiresNameString,
        code: 3009,
        message: "`@musi.lang` requires string `name` arg",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrMusiIntrinsicRequiresOpcodeString,
        code: 3010,
        message: "`@musi.intrinsic` requires string `opcode` arg",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrLinkRequiresForeignLet,
        code: 3011,
        message: "`@link` requires foreign `let` target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrDataLayoutRequiresDataTarget,
        code: 3012,
        message: "data layout attr requires data target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrForeignRequiresForeignLet,
        code: 3013,
        message: "foreign attr requires foreign `let` target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrLinkRequiresStringValue,
        code: 3014,
        message: "`@link` requires string values",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrWhenRequiresStringValue,
        code: 3015,
        message: "`@when` requires string values",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AttrWhenRequiresStringList,
        code: 3016,
        message: "`@when` requires string list for `feature`",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ForeignSignatureRequired,
        code: 3017,
        message: "foreign signature required",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidFfiType,
        code: 3018,
        message: "invalid ffi type",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::LawMustBePure,
        code: 3019,
        message: "law must be pure",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CollectDuplicateDataVariant,
        code: 3020,
        message: "duplicate data variant",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CollectDuplicateEffectOp,
        code: 3021,
        message: "duplicate effect op",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CollectDuplicateEffectLaw,
        code: 3022,
        message: "duplicate effect law",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CollectDuplicateClassMember,
        code: 3023,
        message: "duplicate class member",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CollectDuplicateClassLaw,
        code: 3024,
        message: "duplicate class law",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnknownExport,
        code: 3025,
        message: "unknown export",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidPerformTarget,
        code: 3026,
        message: "invalid perform target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnknownEffect,
        code: 3027,
        message: "unknown effect",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::DuplicateHandlerClause,
        code: 3028,
        message: "duplicate handler clause",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnknownEffectOp,
        code: 3029,
        message: "unknown effect op",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::HandlerClauseArityMismatch,
        code: 3030,
        message: "handler clause arity mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::HandleRequiresSingleValueClause,
        code: 3031,
        message: "handle requires exactly one value clause",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::HandlerMissingOperationClause,
        code: 3032,
        message: "handler missing operation clause",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ResumeOutsideHandlerClause,
        code: 3033,
        message: "resume outside handler clause",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::EffectNotDeclared,
        code: 3034,
        message: "effect not declared",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InstanceMemberArityMismatch,
        code: 3035,
        message: "instance member arity mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnknownInstanceMember,
        code: 3036,
        message: "unknown instance member",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InstanceMemberValueRequired,
        code: 3037,
        message: "instance member value required",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::DuplicateInstanceMember,
        code: 3038,
        message: "duplicate instance member",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::MissingInstanceMember,
        code: 3039,
        message: "missing instance member",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidInstanceTarget,
        code: 3040,
        message: "invalid instance target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::SealedClass,
        code: 3041,
        message: "sealed class",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnknownClass,
        code: 3042,
        message: "unknown class",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::DuplicateInstance,
        code: 3043,
        message: "duplicate instance",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::PlainLetRequiresIrrefutablePattern,
        code: 3044,
        message: "plain `let` requires irrefutable pattern",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ModuleDestructuringRequiresStaticModule,
        code: 3045,
        message: "module destructuring requires static module value",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::RecordDestructuringRequiresRecordOrModule,
        code: 3046,
        message: "record destructuring requires record or module value",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CallableLetRequiresSimpleBindingPattern,
        code: 3047,
        message: "callable `let` requires simple binding pattern",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ArraySpreadRequiresOneDimensionalArray,
        code: 3048,
        message: "array spread requires one-dimensional array",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidArraySpreadSource,
        code: 3049,
        message: "invalid array spread source",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidRecordSpreadSource,
        code: 3050,
        message: "invalid record spread source",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::DuplicateRecordField,
        code: 3051,
        message: "duplicate record field",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::VariantMissingDataContext,
        code: 3052,
        message: "variant constructor requires data type context",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::VariantConstructorArityMismatch,
        code: 3053,
        message: "variant constructor arity mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidVariantArity,
        code: 3054,
        message: "invalid variant arity",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnknownDataVariant,
        code: 3055,
        message: "unknown data variant",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::RecordLiteralRequiresNamedFields,
        code: 3056,
        message: "record literal requires named fields",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ArrayLiteralLengthUnknownFromRuntimeSpread,
        code: 3057,
        message: "array literal length unknown from runtime spread",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ArrayLiteralLengthMismatch,
        code: 3058,
        message: "array literal length mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::SumConstructorArityMismatch,
        code: 3059,
        message: "sum constructor arity mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidIndexArity,
        code: 3060,
        message: "invalid index arity",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidCallTarget,
        code: 3061,
        message: "invalid call target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CallArityMismatch,
        code: 3062,
        message: "call arity mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidTypeApplication,
        code: 3063,
        message: "invalid type application",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidCallSpreadSource,
        code: 3064,
        message: "invalid call spread source",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CallRuntimeSpreadRequiresArrayAny,
        code: 3065,
        message: "call runtime spread requires `[]Any`",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::CallSpreadRequiresTupleOrArray,
        code: 3066,
        message: "call spread requires tuple or array",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::DeclarationUsedAsValue,
        code: 3067,
        message: "declaration form used as value",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::TargetGateRejected,
        code: 3068,
        message: "target gate rejected item",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidIndexTarget,
        code: 3069,
        message: "invalid index target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::IndexRequiresArgument,
        code: 3070,
        message: "index requires one or more args",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnknownField,
        code: 3071,
        message: "unknown field",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidFieldAccess,
        code: 3072,
        message: "invalid field access",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidOptionalFieldAccess,
        code: 3073,
        message: "invalid optional field access",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidRecordUpdateTarget,
        code: 3074,
        message: "invalid record update target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::MutForbiddenInTypeTestTarget,
        code: 3075,
        message: "`mut` not allowed in type test target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::MutForbiddenInTypeCastTarget,
        code: 3076,
        message: "`mut` not allowed in type cast target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::WriteRequiresMutValue,
        code: 3077,
        message: "write requires `mut T`",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::WriteRequiresMutArray,
        code: 3078,
        message: "write requires `mut []T`",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::WriteRequiresMutRecord,
        code: 3079,
        message: "write requires `mut { ... }`",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidFieldUpdateTarget,
        code: 3080,
        message: "invalid field update target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnsupportedAssignmentTarget,
        code: 3081,
        message: "unsupported assignment target",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::NumericOperandRequired,
        code: 3082,
        message: "numeric operand required",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::BinaryOperatorHasNoExecutableLowering,
        code: 3083,
        message: "binary operator has no executable lowering",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::TypeMismatch,
        code: 3084,
        message: "type mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::InvalidTypeExpression,
        code: 3085,
        message: "invalid type expression",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::TypeApplicationArityMismatch,
        code: 3086,
        message: "type application arity mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ArrayTypeRequiresItem,
        code: 3087,
        message: "Array requires at least one arg",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AmbiguousVariantTag,
        code: 3088,
        message: "ambiguous variant tag; add type annotation",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::VariantPatternArityMismatch,
        code: 3089,
        message: "variant pattern arity mismatch",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::OrPatternBindersMismatch,
        code: 3090,
        message: "or-pattern binders must match",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::UnsatisfiedConstraint,
        code: 3091,
        message: "unsatisfied constraint",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::AmbiguousInstanceMatch,
        code: 3092,
        message: "ambiguous instance match",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ConstrainedNonCallableBinding,
        code: 3093,
        message: "non-callable `let` cannot have `where` constraints",
    },
    SemaDiagInfo {
        kind: SemaDiagKind::ExportedCallableRequiresConcreteConstraints,
        code: 3094,
        message: "exported callable requires fully resolved constraints",
    },
];
