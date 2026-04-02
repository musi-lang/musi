use music_base::SourceId;
use music_module::ModuleKey;
use music_names::{Interner, NameBindingKind, NameSite};
use music_syntax::{Lexer, SyntaxNodeKind, canonical_name_text, parse};

use crate::{ResolveOptions, resolve_module};

fn find_nth_name_site(
    source_id: SourceId,
    tree: &music_syntax::SyntaxTree<'_>,
    spelling: &str,
    nth: usize,
) -> Option<NameSite> {
    let mut hits = 0usize;
    let mut stack = vec![tree.root()];
    while let Some(node) = stack.pop() {
        if node.kind() == SyntaxNodeKind::NameExpr {
            let tok = node.child_tokens().find(|t| {
                matches!(
                    t.kind(),
                    music_syntax::TokenKind::Ident | music_syntax::TokenKind::OpIdent
                )
            });
            if let Some(tok) = tok {
                if let Some(raw) = tok.text() {
                    let canon = canonical_name_text(tok.kind(), raw);
                    if canon == spelling {
                        if hits == nth {
                            return Some(NameSite::new(source_id, tok.span()));
                        }
                        hits += 1;
                    }
                }
            }
        }
        for child in node.child_nodes() {
            stack.push(child);
        }
    }
    None
}

#[test]
fn resolves_let_name_use() {
    let src = "let x := 1; x;";
    let source_id = SourceId::from_raw(1);
    let module_key = ModuleKey::new("main");
    let parsed = parse(Lexer::new(src).lex());
    assert!(parsed.errors().is_empty());

    let mut interner = Interner::new();
    let resolved = resolve_module(
        source_id,
        &module_key,
        parsed.tree(),
        &mut interner,
        ResolveOptions::default(),
    );
    let site = find_nth_name_site(source_id, parsed.tree(), "x", 0).expect("use site");
    let binding_id = resolved.names.refs.get(&site).copied().expect("binding");
    let binding = resolved.names.bindings.get(binding_id);
    assert_eq!(binding.kind, NameBindingKind::Let);
    assert_eq!(interner.resolve(binding.name), "x");
}

#[test]
fn resolves_rec_name_use_in_rhs() {
    let src = "let rec f := f;";
    let source_id = SourceId::from_raw(2);
    let module_key = ModuleKey::new("main");
    let parsed = parse(Lexer::new(src).lex());
    assert!(parsed.errors().is_empty());

    let mut interner = Interner::new();
    let resolved = resolve_module(
        source_id,
        &module_key,
        parsed.tree(),
        &mut interner,
        ResolveOptions::default(),
    );
    let site = find_nth_name_site(source_id, parsed.tree(), "f", 0).expect("use site");
    let binding_id = resolved.names.refs.get(&site).copied().expect("binding");
    let binding = resolved.names.bindings.get(binding_id);
    assert_eq!(binding.kind, NameBindingKind::Let);
    assert_eq!(interner.resolve(binding.name), "f");
}

#[test]
fn resolves_case_pat_binder_in_arm() {
    let src = "let x := 0; case x of (| .Some(y) => y | _ => x);";
    let source_id = SourceId::from_raw(3);
    let module_key = ModuleKey::new("main");
    let parsed = parse(Lexer::new(src).lex());
    assert!(parsed.errors().is_empty());

    let mut interner = Interner::new();
    let resolved = resolve_module(
        source_id,
        &module_key,
        parsed.tree(),
        &mut interner,
        ResolveOptions::default(),
    );
    let y_site = find_nth_name_site(source_id, parsed.tree(), "y", 0).expect("y use site");
    let y_binding = resolved
        .names
        .refs
        .get(&y_site)
        .copied()
        .expect("y binding");
    assert_eq!(
        resolved.names.bindings.get(y_binding).kind,
        NameBindingKind::PatternBind
    );

    let x_site = find_nth_name_site(source_id, parsed.tree(), "x", 0).expect("x use site");
    let x_binding = resolved
        .names
        .refs
        .get(&x_site)
        .copied()
        .expect("x binding");
    assert_eq!(
        resolved.names.bindings.get(x_binding).kind,
        NameBindingKind::Let
    );
}

#[test]
fn resolves_lambda_param_in_body() {
    let src = "(x: Int) => x;";
    let source_id = SourceId::from_raw(4);
    let module_key = ModuleKey::new("main");
    let parsed = parse(Lexer::new(src).lex());
    assert!(parsed.errors().is_empty());

    let mut interner = Interner::new();
    let resolved = resolve_module(
        source_id,
        &module_key,
        parsed.tree(),
        &mut interner,
        ResolveOptions::default(),
    );
    let site = find_nth_name_site(source_id, parsed.tree(), "x", 0).expect("x use site");
    let binding = resolved.names.refs.get(&site).copied().expect("binding");
    assert_eq!(
        resolved.names.bindings.get(binding).kind,
        NameBindingKind::Param
    );
}

#[test]
fn resolves_pi_binder_in_ret() {
    let src = "(x: Type) -> x;";
    let source_id = SourceId::from_raw(5);
    let module_key = ModuleKey::new("main");
    let parsed = parse(Lexer::new(src).lex());
    assert!(parsed.errors().is_empty());

    let mut interner = Interner::new();
    let resolved = resolve_module(
        source_id,
        &module_key,
        parsed.tree(),
        &mut interner,
        ResolveOptions::default(),
    );
    let site = find_nth_name_site(source_id, parsed.tree(), "x", 0).expect("x use site");
    let binding = resolved.names.refs.get(&site).copied().expect("binding");
    assert_eq!(
        resolved.names.bindings.get(binding).kind,
        NameBindingKind::PiBinder
    );
}
