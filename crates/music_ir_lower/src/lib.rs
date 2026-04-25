// crate-private modules share lowering internals; `pub` would widen rscheck public API.
#![allow(clippy::redundant_pub_crate)]

use std::collections::{BTreeMap, HashMap, HashSet};

use music_arena::SliceRange;
use music_base::diag::{Diag, DiagContext};
use music_hir::{
    HirArg, HirArrayItem, HirBinaryOp, HirDim, HirExprId, HirExprKind, HirHandleClause, HirLetMods,
    HirLitId, HirLitKind, HirMatchArm, HirParam, HirPartialRangeKind, HirPatId, HirPatKind,
    HirPrefixOp, HirQuoteKind, HirRecordItem, HirRecordPatField, HirSpliceKind, HirTemplatePart,
    HirTyField, HirTyId, HirTyKind,
};
use music_module::ModuleKey;
use music_names::{Ident, Interner, NameBindingId, NameSite, Symbol};
use music_sema::{
    ComptimeValue, ConstraintAnswer, ConstraintKey, DefinitionKey, ExportedValue, ExprMemberKind,
    SemaDataVariantDef, SemaModule,
};

use music_ir::IrDiagKind;
use music_ir::{
    IrArg, IrAssignTarget, IrBinaryOp, IrCallable, IrCasePattern, IrCaseRecordField, IrDataDef,
    IrDataVariantDef, IrDiagList, IrEffectDef, IrEffectOpDef, IrExpr, IrExprKind, IrForeignDef,
    IrGivenDef, IrGlobal, IrHandleOp, IrIntrinsicKind, IrLit, IrMatchArm as IrLoweredMatchArm,
    IrModule, IrModuleInitPart, IrModuleParts, IrNameRef, IrOrigin, IrParam, IrRangeKind,
    IrRecordField, IrRecordLayoutField, IrSeqPart, IrShapeDef, IrTempId,
};

pub(crate) mod access;
pub(crate) mod array;
pub(crate) mod assign;
pub(crate) mod binary;
pub(crate) mod bindings;
pub(crate) mod call;
pub(crate) mod closures;
pub(crate) mod collect;
pub(crate) mod comptime;
pub(crate) mod constraint_answers;
pub(crate) mod context;
pub(crate) mod destructure;
pub(crate) mod effects;
pub(crate) mod expr;
pub(crate) mod foreign;
pub(crate) mod lower_errors;
pub(crate) mod meta;
pub(crate) mod module;
pub(crate) mod pats;
pub(crate) mod range;
pub(crate) mod record;
pub(crate) mod rewrite;
pub(crate) mod toplevel;
pub(crate) mod type_values;
pub(crate) mod types;
pub(crate) mod validate;
pub(crate) mod variants;

use access::{lower_field_expr, lower_index_expr, record_layout_for_ty};
use closures::{
    ClosureCallableInput, lower_closure_callable, lower_lambda_expr, lower_local_callable_let,
    lower_named_params,
};
use comptime::lower_comptime_value;
use constraint_answers::{
    bind_expr_constraint_answers, hidden_constraint_answer_params_for_binding,
    hidden_constraint_answer_params_for_keys, lower_constraint_answer_expr,
    pop_constraint_answer_bindings, push_constraint_answer_bindings,
};
use context::{
    BoundNameSet, ConstraintAnswerBindingMap, HirParamRange, HirRecordItemRange, LetItemInput,
    LowerCtx, LoweredMatchArmList, LoweringResult, RecordLayout, TopLevelItems, qualified_name,
};
use effects::{lower_answer_literal_expr, lower_handle_expr, lower_request_expr};
use foreign::lower_foreign_let;
use pats::{collect_pattern_bindings, lower_match_expr};
use range::{lower_in_expr, lower_partial_range_expr, lower_range_expr};
use rewrite::rewrite_recursive_binding_refs;
use types::{
    is_builtin_type_name_symbol, render_named_ty_name, render_ty_name, render_type_value_expr_name,
};

use expr::{decl_binding_id, fresh_temp, lower_boxed_expr, lower_expr, use_binding_id};
pub use module::lower_module;
use module::lowering_invariant_violation;
use type_values::is_type_value_expr;
use variants::lower_variant_expr;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
