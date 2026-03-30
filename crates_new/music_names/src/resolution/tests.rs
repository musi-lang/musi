use music_basic::{SourceMap, Span};

use super::{NameBinding, NameBindingKind, NameResolution, NameSite};
use crate::Interner;

#[test]
fn records_refs_by_site() {
    let mut sources = SourceMap::new();
    let source_id = sources.add("test.ms", "x");

    let mut interner = Interner::new();
    let sym = interner.intern("x");

    let mut res = NameResolution::new();
    let site = NameSite::new(source_id, Span::new(0, 1));
    let id = res.alloc_binding(NameBinding {
        name: sym,
        site,
        kind: NameBindingKind::Let,
    });
    res.record_ref(site, id);

    assert_eq!(res.refs.get(&site).copied(), Some(id));
}
