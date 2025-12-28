use musi_basic::interner::Interner;
use musi_sema::SymbolTable;
use musi_sema::symbol::SymbolId;
use musi_sema::ty_repr::{FloatWidth, IntWidth, TyParamId, TyRepr, TyReprKind};

pub fn format_type(ty: &TyRepr, symbols: &SymbolTable, interner: &Interner) -> String {
    match &ty.kind {
        TyReprKind::Int(w) => format!("Int{}", format_int_width(*w)),
        TyReprKind::Nat(w) => format!("Nat{}", format_int_width(*w)),
        TyReprKind::Float(w) => format!("Float{}", format_float_width(*w)),
        TyReprKind::Bool => "Bool".to_owned(),
        TyReprKind::Rune => "Rune".to_owned(),
        TyReprKind::String => "String".to_owned(),
        TyReprKind::Unit => "()".to_owned(),
        TyReprKind::Never => "Never".to_owned(),
        TyReprKind::Any => "Any".to_owned(),
        TyReprKind::Unknown => "Unknown".to_owned(),
        TyReprKind::Tuple(elems) => format_tuple(elems, symbols, interner),
        TyReprKind::Array(elem, size) => format_array(elem, *size, symbols, interner),
        TyReprKind::Ptr(inner) => format!("^{}", format_type(inner, symbols, interner)),
        TyReprKind::Optional(inner) => format!("?{}", format_type(inner, symbols, interner)),
        TyReprKind::Fn(params, ret) => format_fn(params, ret, symbols, interner),
        TyReprKind::Named(sym_id, args) => format_named(*sym_id, args, symbols, interner),
        TyReprKind::Var(id) => format!("?T{}", id.as_u32()),
        TyReprKind::Poly { params, body } => format_poly(params, body, symbols, interner),
        TyReprKind::TypeParam(id) => format!("T{}", id.as_u32()),
        TyReprKind::Error => "<error>".to_owned(),
    }
}

const fn format_int_width(w: IntWidth) -> u8 {
    w.bits()
}

const fn format_float_width(w: FloatWidth) -> u8 {
    w.bits()
}

fn format_tuple(elems: &[TyRepr], symbols: &SymbolTable, interner: &Interner) -> String {
    let formatted: Vec<_> = elems
        .iter()
        .map(|e| format_type(e, symbols, interner))
        .collect();
    format!("({})", formatted.join(", "))
}

fn format_array(
    elem: &TyRepr,
    size: Option<usize>,
    symbols: &SymbolTable,
    interner: &Interner,
) -> String {
    let elem_str = format_type(elem, symbols, interner);
    size.map_or_else(|| format!("[]{elem_str}"), |n| format!("[{n}]{elem_str}"))
}

fn format_fn(
    params: &[TyRepr],
    ret: &TyRepr,
    symbols: &SymbolTable,
    interner: &Interner,
) -> String {
    let params_str: Vec<_> = params
        .iter()
        .map(|p| format_type(p, symbols, interner))
        .collect();
    let ret_str = format_type(ret, symbols, interner);
    format!("({}) -> {ret_str}", params_str.join(", "))
}

fn format_named(
    sym_id: SymbolId,
    args: &[TyRepr],
    symbols: &SymbolTable,
    interner: &Interner,
) -> String {
    let name = symbols.get(sym_id).map_or_else(
        || format!("<unknown#{}>", sym_id.0),
        |sym| interner.resolve(sym.name.id).to_owned(),
    );

    if args.is_empty() {
        name
    } else {
        let args_str: Vec<_> = args
            .iter()
            .map(|a| format_type(a, symbols, interner))
            .collect();
        format!("{name}[{}]", args_str.join(", "))
    }
}

fn format_poly(
    params: &[TyParamId],
    body: &TyRepr,
    symbols: &SymbolTable,
    interner: &Interner,
) -> String {
    let params_str: Vec<_> = params.iter().map(|p| format!("T{}", p.as_u32())).collect();
    let body_str = format_type(body, symbols, interner);
    format!("forall[{}]. {body_str}", params_str.join(", "))
}
