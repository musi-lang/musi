use std::collections::{HashMap, HashSet};

use music_base::Span;
use music_hir::{
    HirArg, HirArrayItem, HirAttr, HirAttrArg, HirBinder, HirConstraint, HirDim, HirEffectItem,
    HirExprId, HirFieldDef, HirHandleClause, HirMatchArm, HirMemberDef, HirParam, HirPatId,
    HirRecordItem, HirRecordPatField, HirTemplatePart, HirTyField, HirTyId, HirVariantDef,
    HirVariantFieldDef, HirVariantPatArg,
};
use music_module::ModuleKey;
use music_names::{Ident, NameBindingId, NameSite, Symbol};

use crate::api::{
    ComptimeValue, ConstraintAnswer, ConstraintKey, DefinitionKey, ExprFacts, ExprMemberFact,
    ForeignLinkInfo, GivenFacts, PatFacts, SemaDataDef, SemaDataVariantDef, SemaEffectDef,
    SemaEffectOpDef, ShapeFacts,
};
use crate::checker::schemes::BindingScheme;
use crate::effects::EffectRow;

use super::ResumeCtx;

pub const SYNTH_SUM_PREFIX: &str = "__sum__";

pub type BindingIdMap = HashMap<NameSite, NameBindingId>;
pub type ImportTargetMap = HashMap<Span, ModuleKey>;
pub type BindingTypeMap = HashMap<NameBindingId, HirTyId>;
pub type BindingEffectsMap = HashMap<NameBindingId, EffectRow>;
pub type BindingSchemeMap = HashMap<NameBindingId, BindingScheme>;
pub type TypeParamKindScope = HashMap<Symbol, HirTyId>;
pub type TypeParamKindScopeList = Vec<TypeParamKindScope>;
pub type BindingConstraintKeyMap = HashMap<NameBindingId, Box<[ConstraintKey]>>;
pub type BindingImportRecordTargetMap = HashMap<NameBindingId, ModuleKey>;
pub type BindingConstIntMap = HashMap<NameBindingId, i64>;
pub type BindingComptimeValueMap = HashMap<NameBindingId, ComptimeValue>;
pub type SealedShapeSet = HashSet<DefinitionKey>;
pub type GatedBindingSet = HashSet<NameBindingId>;
pub type ForeignLinkMap = HashMap<NameBindingId, ForeignLinkInfo>;
pub type UnsafeBindingSet = HashSet<NameBindingId>;
pub type AttachedMethodMap = HashMap<Symbol, Vec<NameBindingId>>;
pub type EffectDefMap = HashMap<Box<str>, EffectDef>;
pub type DataDefMap = HashMap<Box<str>, DataDef>;
pub type ShapeIndexMap = HashMap<Symbol, HirExprId>;
pub type ShapeFactsByNameMap = HashMap<Symbol, ShapeFacts>;
pub type ShapeFactsMap = HashMap<HirExprId, ShapeFacts>;
pub type GivenFactsMap = HashMap<HirExprId, GivenFacts>;
pub type ExprFactsList = Vec<ExprFacts>;
pub type PatFactsList = Vec<PatFacts>;
pub type ExprCallableEffectsMap = HashMap<HirExprId, EffectRow>;
pub type ExprImportRecordTargetMap = HashMap<HirExprId, ModuleKey>;
pub type TypeTestTargetMap = HashMap<HirExprId, HirTyId>;
pub type ExprConstraintAnswerMap = HashMap<HirExprId, Box<[ConstraintAnswer]>>;
pub type ExprDotCallableBindingMap = HashMap<HirExprId, NameBindingId>;
pub type ExprMemberFactMap = HashMap<HirExprId, ExprMemberFact>;
pub type ExprComptimeValueMap = HashMap<HirExprId, ComptimeValue>;
pub type ResumeCtxList = Vec<ResumeCtx>;
pub type ExpectedTyList = Vec<HirTyId>;
pub type ConstraintAnswerScope = HashMap<ConstraintKey, ConstraintAnswer>;
pub type ConstraintAnswerScopeList = Vec<ConstraintAnswerScope>;
pub type StaticImportList = Vec<ModuleKey>;
pub type ExprIdList = Vec<HirExprId>;
pub type ArgList = Vec<HirArg>;
pub type DimList = Vec<HirDim>;
pub type TyIdList = Vec<HirTyId>;
pub type TyFieldList = Vec<HirTyField>;
pub type ArrayItemList = Vec<HirArrayItem>;
pub type RecordItemList = Vec<HirRecordItem>;
pub type ParamList = Vec<HirParam>;
pub type AttrList = Vec<HirAttr>;
pub type AttrArgList = Vec<HirAttrArg>;
pub type MemberDefList = Vec<HirMemberDef>;
pub type HandleClauseList = Vec<HirHandleClause>;
pub type MatchArmList = Vec<HirMatchArm>;
pub type ConstraintList = Vec<HirConstraint>;
pub type VariantDefList = Vec<HirVariantDef>;
pub type VariantFieldDefList = Vec<HirVariantFieldDef>;
pub type FieldDefList = Vec<HirFieldDef>;
pub type EffectItemList = Vec<HirEffectItem>;
pub type PatIdList = Vec<HirPatId>;
pub type RecordPatFieldList = Vec<HirRecordPatField>;
pub type VariantPatArgList = Vec<HirVariantPatArg>;
pub type IdentList = Vec<Ident>;
pub type BinderList = Vec<HirBinder>;
pub type TemplatePartList = Vec<HirTemplatePart>;
pub type TypeParamKindList = Vec<(Symbol, HirTyId)>;

pub type EffectOpDef = SemaEffectOpDef;
pub type EffectDef = SemaEffectDef;
pub type DataVariantDef = SemaDataVariantDef;
pub type DataDef = SemaDataDef;
