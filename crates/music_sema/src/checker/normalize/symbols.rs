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
    pub fn named_type_for_symbol(&mut self, symbol: Symbol) -> HirTyId {
        let known = self.known();
        let builtins = self.builtins();
        if symbol == known.type_ || self.is_universe_symbol(symbol) {
            return builtins.type_;
        }
        if let Some(ty) = simple_named_type_for_symbol(known, builtins, symbol) {
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
}
