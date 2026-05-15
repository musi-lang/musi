use music_hir::{HirTyId, HirTyKind};
use music_names::{KnownSymbols, Symbol};

use crate::checker::PassBase;
use crate::checker::state::Builtins;

fn simple_named_type_for_symbol(
    known: KnownSymbols,
    builtins: Builtins,
    symbol: Symbol,
) -> Option<HirTyId> {
    [
        (known.any, builtins.any),
        (known.unknown, builtins.unknown),
        (known.syntax, builtins.syntax),
        (known.empty, builtins.empty),
        (known.unit, builtins.unit),
        (known.bool_, builtins.bool_),
        (known.nat, builtins.nat),
        (known.int_, builtins.int_),
        (known.int8, builtins.int8),
        (known.int16, builtins.int16),
        (known.int32, builtins.int32),
        (known.int64, builtins.int64),
        (known.nat8, builtins.nat8),
        (known.nat16, builtins.nat16),
        (known.nat32, builtins.nat32),
        (known.nat64, builtins.nat64),
        (known.float_, builtins.float_),
        (known.float32, builtins.float32),
        (known.float64, builtins.float64),
        (known.string_, builtins.string_),
        (known.rune, builtins.rune),
        (known.cstring, builtins.cstring),
        (known.cptr, builtins.cptr),
    ]
    .into_iter()
    .find_map(|(known_symbol, ty)| (symbol == known_symbol).then_some(ty))
}

impl PassBase<'_, '_, '_> {
    pub(in crate::checker) fn builtin_type_alias_for_name(
        &mut self,
        name: &str,
    ) -> Option<HirTyId> {
        let builtins = self.builtins();
        if let Some(width) = self.word_width_for_name(name) {
            return Some(self.alloc_ty(HirTyKind::Bits { width }));
        }
        [
            ("Nat", builtins.nat),
            ("Int", builtins.int_),
            ("Int8", builtins.int8),
            ("Int16", builtins.int16),
            ("Int32", builtins.int32),
            ("Int64", builtins.int64),
            ("Nat8", builtins.nat8),
            ("Nat16", builtins.nat16),
            ("Nat32", builtins.nat32),
            ("Nat64", builtins.nat64),
            ("Bit", builtins.bool_),
            ("CChar", builtins.int8),
            ("CSChar", builtins.int8),
            ("CUChar", builtins.nat8),
            ("CShort", builtins.int16),
            ("CUShort", builtins.nat16),
            ("CInt", builtins.int32),
            ("CUInt", builtins.nat32),
            ("CLong", builtins.int_),
            ("CULong", builtins.nat),
            ("CLongLong", builtins.int64),
            ("CULongLong", builtins.nat64),
            ("CSize", builtins.nat),
            ("CSizeDiff", builtins.int_),
            ("CFloat", builtins.float32),
            ("CDouble", builtins.float64),
            ("CString", builtins.cstring),
            ("CPtr", builtins.cptr),
            ("char", builtins.int8),
            ("bool", builtins.bool_),
            ("int8_t", builtins.int8),
            ("int16_t", builtins.int16),
            ("int32_t", builtins.int32),
            ("int64_t", builtins.int64),
            ("uint8_t", builtins.nat8),
            ("uint16_t", builtins.nat16),
            ("uint32_t", builtins.nat32),
            ("uint64_t", builtins.nat64),
            ("intptr_t", builtins.int_),
            ("uintptr_t", builtins.nat),
            ("size_t", builtins.nat),
            ("ptrdiff_t", builtins.int_),
        ]
        .into_iter()
        .find_map(|(alias, ty)| (name == alias).then_some(ty))
    }

    pub fn named_type_for_symbol(&mut self, symbol: Symbol) -> HirTyId {
        let known = self.known();
        let builtins = self.builtins();
        if symbol == known.type_ || self.is_universe_symbol(symbol) {
            return builtins.type_;
        }
        if let Some(ty) = simple_named_type_for_symbol(known, builtins, symbol) {
            return ty;
        }
        let symbol_name = self.resolve_symbol(symbol).to_owned();
        if let Some(ty) = self.builtin_type_alias_for_name(&symbol_name) {
            return ty;
        }
        if let Some(ty) = self.type_alias(symbol) {
            return ty;
        }
        let args = self.alloc_ty_list(Vec::<HirTyId>::new());
        self.alloc_ty(HirTyKind::Named { name: symbol, args })
    }

    pub(super) fn is_universe_symbol(&self, symbol: Symbol) -> bool {
        let Some(rest) = self.resolve_symbol(symbol).strip_prefix("Type") else {
            return false;
        };
        !rest.is_empty() && rest.bytes().all(|byte| byte.is_ascii_digit())
    }

    pub fn symbol_value_type(&self, symbol: Symbol) -> HirTyId {
        let known = self.known();
        let builtins = self.builtins();
        if [
            known.type_,
            known.array,
            known.any,
            known.unknown,
            known.syntax,
            known.empty,
            known.unit,
            known.bool_,
            known.bits,
            known.word,
            known.word8,
            known.word16,
            known.word32,
            known.word64,
            known.range,
            known.pin,
            known.closed_range,
            known.partial_range_from,
            known.partial_range_up_to,
            known.partial_range_thru,
            known.nat,
            known.int_,
            known.int8,
            known.int16,
            known.int32,
            known.int64,
            known.nat8,
            known.nat16,
            known.nat32,
            known.nat64,
            known.float_,
            known.float32,
            known.float64,
            known.string_,
            known.rune,
            known.cstring,
            known.cptr,
        ]
        .contains(&symbol)
        {
            builtins.type_
        } else {
            builtins.unknown
        }
    }

    fn word_width_for_name(&self, name: &str) -> Option<u32> {
        match name {
            "Word" => self
                .target()
                .and_then(|target| target.pointer_width)
                .map(u32::from),
            "Word8" => Some(8),
            "Word16" => Some(16),
            "Word32" => Some(32),
            "Word64" => Some(64),
            _ => None,
        }
    }
}
