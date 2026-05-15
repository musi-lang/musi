use music_base::{NumericSuffixClass, parse_i64_literal, parse_u64_literal, split_numeric_suffix};
use music_hir::{HirLitId, HirLitKind, HirTyId, HirTyKind};

use crate::api::ExprFacts;

use super::super::{CheckPass, state::Builtins};

impl CheckPass<'_, '_, '_> {
    pub(super) fn check_lit_expr(&self, lit: HirLitId) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        let ty = match ctx.lit_kind(lit) {
            HirLitKind::Int { raw } => Self::numeric_literal_suffix_ty(raw.as_ref(), builtins)
                .unwrap_or_else(|| {
                    ctx.expected_ty().map_or(builtins.int_, |expected| {
                        ctx.int_lit_ty_for_expected(raw.as_ref(), expected)
                            .unwrap_or(builtins.int_)
                    })
                }),
            HirLitKind::Rune { .. } => builtins.rune,
            HirLitKind::Float { raw } => {
                let suffix_ty =
                    Self::numeric_literal_suffix_ty(raw.as_ref(), builtins).filter(|ty| {
                        matches!(
                            ctx.ty(*ty).kind,
                            HirTyKind::Float | HirTyKind::Float32 | HirTyKind::Float64
                        )
                    });
                suffix_ty.unwrap_or_else(|| {
                    ctx.expected_ty().map_or(builtins.float_, |expected| {
                        match ctx.ty(expected).kind {
                            HirTyKind::Float32 | HirTyKind::Float64 | HirTyKind::Float => expected,
                            _ => builtins.float_,
                        }
                    })
                })
            }
            HirLitKind::String { .. } => {
                ctx.expected_ty()
                    .map_or(builtins.string_, |expected| match ctx.ty(expected).kind {
                        HirTyKind::CString | HirTyKind::String => expected,
                        _ => builtins.string_,
                    })
            }
        };
        ExprFacts::new(ty)
    }

    fn int_lit_ty_for_expected(&self, raw: &str, expected: HirTyId) -> Option<HirTyId> {
        let signed = parse_i64_literal(raw).map(i128::from);
        let unsigned = parse_u64_literal(raw).map(i128::from);
        let ok = match self.ty(expected).kind {
            HirTyKind::Int8 => signed.is_some_and(|value| i8::try_from(value).is_ok()),
            HirTyKind::Int16 => signed.is_some_and(|value| i16::try_from(value).is_ok()),
            HirTyKind::Int32 => signed.is_some_and(|value| i32::try_from(value).is_ok()),
            HirTyKind::Int64 | HirTyKind::Int => {
                signed.is_some_and(|value| i64::try_from(value).is_ok())
            }
            HirTyKind::Nat8 => unsigned.is_some_and(|value| u8::try_from(value).is_ok()),
            HirTyKind::Nat16 => unsigned.is_some_and(|value| u16::try_from(value).is_ok()),
            HirTyKind::Nat32 => unsigned.is_some_and(|value| u32::try_from(value).is_ok()),
            HirTyKind::Nat64 | HirTyKind::Nat => unsigned.is_some(),
            _ => false,
        };
        ok.then_some(expected)
    }

    fn numeric_literal_suffix_ty(raw: &str, builtins: Builtins) -> Option<HirTyId> {
        let (_, suffix) = split_numeric_suffix(raw);
        let suffix = suffix?;
        match suffix.class {
            NumericSuffixClass::Z => Some(match suffix.width {
                Some(8) => builtins.int8,
                Some(16) => builtins.int16,
                Some(32) => builtins.int32,
                Some(64) => builtins.int64,
                _ => builtins.int_,
            }),
            NumericSuffixClass::N => Some(match suffix.width {
                Some(8) => builtins.nat8,
                Some(16) => builtins.nat16,
                Some(32) => builtins.nat32,
                Some(64) => builtins.nat64,
                _ => builtins.nat,
            }),
            NumericSuffixClass::F => Some(match suffix.width {
                Some(32) => builtins.float32,
                Some(64) => builtins.float64,
                _ => builtins.float_,
            }),
        }
    }
}
