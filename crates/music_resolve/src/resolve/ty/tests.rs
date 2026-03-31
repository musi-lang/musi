use music_basic::SourceMap;
use music_hir::{HirDim, HirExprKind, HirTyKind};
use music_known::KnownSymbols;
use music_lex::Lexer;
use music_names::Interner;

use crate::{ResolveOptions, resolve_module};

#[test]
fn test_array_dim_int_lit_overflow_is_none() {
    let src = "let x : [18446744073709551616]Int := 0;";
    let mut sources = SourceMap::new();
    let source_id = sources.add("test.ms", src);

    let lexed = Lexer::new(src).lex();
    let parsed = music_parse::parse(source_id, &lexed);

    let mut interner = Interner::new();
    let known = KnownSymbols::new(&mut interner);
    let resolved = resolve_module(
        parsed.tree(),
        &sources,
        &mut interner,
        ResolveOptions {
            prelude: known.compiler_prelude().to_vec(),
            import_env: None,
        },
    );

    let root = resolved
        .module
        .store
        .exprs
        .get(resolved.module.root)
        .clone();
    let HirExprKind::Sequence { exprs, .. } = root.kind else {
        panic!("expected root sequence");
    };
    let first = exprs.first().copied().expect("non-empty root");
    let first = resolved.module.store.exprs.get(first).clone();
    let HirExprKind::Let {
        annot: Some(ty), ..
    } = first.kind
    else {
        panic!("expected let with annotation");
    };
    let ty = resolved.module.store.tys.get(ty).clone();
    let HirTyKind::Array { dims, .. } = ty.kind else {
        panic!("expected array type");
    };

    assert!(
        matches!(dims.first(), Some(HirDim::IntLit { value: None, .. })),
        "expected overflowing dim literal to be preserved as None, got {dims:?}"
    );
}
