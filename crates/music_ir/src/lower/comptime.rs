use music_base::{SourceId, Span};
use music_module::ModuleKey;
use music_sema::{
    ComptimeClassValue, ComptimeClosureValue, ComptimeDataValue, ComptimeEffectValue,
    ComptimeForeignValue, ComptimeModuleValue, ComptimeSeqValue, ComptimeTypeValue, ComptimeValue,
    DefinitionKey,
};
use music_term::{TypeModuleRef, TypeTerm, TypeTermKind};

use crate::api::{IrExpr, IrExprKind, IrLit, IrNameRef, IrOrigin};

use super::{LowerCtx, invalid_lowering_path};

pub(super) fn lower_comptime_value(ctx: &mut LowerCtx<'_>, value: &ComptimeValue) -> IrExprKind {
    match value {
        ComptimeValue::Unit => IrExprKind::Unit,
        ComptimeValue::Int(value) => int_lit(*value),
        ComptimeValue::Nat(value) => nat_lit(*value),
        ComptimeValue::Float(raw) => IrExprKind::Lit(IrLit::Float { raw: raw.clone() }),
        ComptimeValue::String(value) => IrExprKind::Lit(IrLit::String {
            value: value.clone(),
        }),
        ComptimeValue::Rune(value) => IrExprKind::Lit(IrLit::Rune { value: *value }),
        ComptimeValue::CPtr(value) => usize_lit(*value),
        ComptimeValue::Syntax(term) => IrExprKind::SyntaxValue {
            raw: format!("quote ({})", term.text()).into_boxed_str(),
        },
        ComptimeValue::Seq(value) => lower_comptime_seq(ctx, value),
        ComptimeValue::Data(value) => lower_comptime_data(ctx, value),
        ComptimeValue::Closure(value) => lower_comptime_closure(ctx, value),
        ComptimeValue::Type(value) => lower_comptime_type(value),
        ComptimeValue::Module(value) => lower_comptime_module(value),
        ComptimeValue::Foreign(value) => lower_comptime_foreign(value),
        ComptimeValue::Continuation(_) => {
            invalid_lowering_path("escaping compile-time continuation")
        }
        ComptimeValue::Effect(value) => lower_comptime_effect(ctx, value),
        ComptimeValue::Class(value) => lower_comptime_class(ctx, value),
    }
}

fn lower_comptime_seq(ctx: &mut LowerCtx<'_>, value: &ComptimeSeqValue) -> IrExprKind {
    IrExprKind::Array {
        ty_name: value.ty.to_string().into_boxed_str(),
        items: value
            .items
            .iter()
            .map(|item| IrExpr::new(dummy_origin(), lower_comptime_value(ctx, item)))
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    }
}

fn lower_comptime_data(ctx: &mut LowerCtx<'_>, value: &ComptimeDataValue) -> IrExprKind {
    let data_key = definition_key_for_type(ctx, &value.ty);
    let data = ctx
        .sema
        .data_defs()
        .find(|data| data.key() == &data_key)
        .or_else(|| ctx.sema.data_def(data_key.name.as_ref()))
        .unwrap_or_else(|| invalid_lowering_path("compile-time data definition missing"));
    let tag_index = data
        .variant_index(value.variant.as_ref())
        .unwrap_or_else(|| invalid_lowering_path("compile-time data variant missing"));
    let args = value
        .fields
        .iter()
        .map(|field| IrExpr::new(dummy_origin(), lower_comptime_value(ctx, field)))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    IrExprKind::VariantNew {
        data_key,
        tag_index,
        tag_value: value.tag,
        field_count: u16::try_from(args.len()).unwrap_or(u16::MAX),
        args,
    }
}

fn lower_comptime_closure(ctx: &mut LowerCtx<'_>, value: &ComptimeClosureValue) -> IrExprKind {
    IrExprKind::ClosureNew {
        callee: IrNameRef::new(value.name.clone()).with_module_target(value.module.clone()),
        captures: value
            .captures
            .iter()
            .map(|capture| IrExpr::new(dummy_origin(), lower_comptime_value(ctx, capture)))
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    }
}

fn lower_comptime_type(value: &ComptimeTypeValue) -> IrExprKind {
    IrExprKind::TypeValue {
        ty_name: value.term.to_string().into_boxed_str(),
    }
}

fn lower_comptime_module(value: &ComptimeModuleValue) -> IrExprKind {
    IrExprKind::ModuleLoad {
        spec: Box::new(IrExpr::new(
            dummy_origin(),
            IrExprKind::Lit(IrLit::String {
                value: value.key.as_str().into(),
            }),
        )),
    }
}

fn lower_comptime_foreign(value: &ComptimeForeignValue) -> IrExprKind {
    let callee = IrExpr::new(
        dummy_origin(),
        IrExprKind::Name {
            binding: None,
            name: value.name.clone(),
            module_target: Some(value.module.clone()),
        },
    );
    if value.type_args.is_empty() {
        return callee.kind;
    }
    IrExprKind::TypeApply {
        callee: Box::new(callee),
        type_args: value
            .type_args
            .iter()
            .map(ToString::to_string)
            .map(String::into_boxed_str)
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    }
}

fn lower_comptime_effect(ctx: &LowerCtx<'_>, value: &ComptimeEffectValue) -> IrExprKind {
    lower_comptime_module_export(ctx, &value.module, &value.name)
}

fn lower_comptime_class(ctx: &LowerCtx<'_>, value: &ComptimeClassValue) -> IrExprKind {
    lower_comptime_module_export(ctx, &value.module, &value.name)
}

fn lower_comptime_module_export(ctx: &LowerCtx<'_>, module: &ModuleKey, name: &str) -> IrExprKind {
    if module == &ctx.module_key {
        invalid_lowering_path("compile-time nominal capability needs module export boundary");
    }
    IrExprKind::ModuleGet {
        base: Box::new(IrExpr::new(
            dummy_origin(),
            IrExprKind::ModuleLoad {
                spec: Box::new(IrExpr::new(
                    dummy_origin(),
                    IrExprKind::Lit(IrLit::String {
                        value: module.as_str().into(),
                    }),
                )),
            },
        )),
        name: name.into(),
    }
}

fn definition_key_for_type(ctx: &LowerCtx<'_>, ty: &TypeTerm) -> DefinitionKey {
    let TypeTermKind::Named { module, name, .. } = &ty.kind else {
        invalid_lowering_path("compile-time data type is not named");
    };
    let (module, name) = split_qualified_type_name(ctx, module.as_ref(), name);
    DefinitionKey::new(module, name)
}

fn split_qualified_type_name(
    ctx: &LowerCtx<'_>,
    module: Option<&TypeModuleRef>,
    name: &str,
) -> (ModuleKey, Box<str>) {
    if let Some((module, name)) = name.rsplit_once("::") {
        return (ModuleKey::new(module), name.into());
    }
    let module = module.map_or_else(
        || ctx.module_key.clone(),
        |module| ModuleKey::new(module.spec.as_ref()),
    );
    (module, name.into())
}

fn int_lit(value: i64) -> IrExprKind {
    IrExprKind::Lit(IrLit::Int {
        raw: value.to_string().into_boxed_str(),
    })
}

fn nat_lit(value: u64) -> IrExprKind {
    IrExprKind::Lit(IrLit::Int {
        raw: value.to_string().into_boxed_str(),
    })
}

fn usize_lit(value: usize) -> IrExprKind {
    IrExprKind::Lit(IrLit::Int {
        raw: value.to_string().into_boxed_str(),
    })
}

const fn dummy_origin() -> IrOrigin {
    IrOrigin::new(SourceId::from_raw(0), Span::DUMMY)
}
