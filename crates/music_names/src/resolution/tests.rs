#![allow(unused_imports)]

use music_base::{SourceId, Span};

use super::{NameBinding, NameBindingKind, NameResolution, NameSite};
use crate::{Symbol, SymbolSlice};

mod success {
    use super::*;

    #[test]
    fn alloc_binding_returns_stable_ids() {
        let mut names = NameResolution::new();
        let a = names.alloc_binding(NameBinding {
            name: Symbol::from_raw(0),
            site: NameSite::new(SourceId::from_raw(0), Span::DUMMY),
            kind: NameBindingKind::Prelude,
        });
        let b = names.alloc_binding(NameBinding {
            name: Symbol::from_raw(1),
            site: NameSite::new(SourceId::from_raw(0), Span::new(1, 2)),
            kind: NameBindingKind::Let,
        });
        assert_eq!(a.raw(), 0);
        assert_eq!(b.raw(), 1);
    }

    #[test]
    fn record_ref_inserts_mapping() {
        let mut names = NameResolution::new();
        let binding = names.alloc_binding(NameBinding {
            name: Symbol::from_raw(0),
            site: NameSite::new(SourceId::from_raw(0), Span::DUMMY),
            kind: NameBindingKind::Prelude,
        });
        let site = NameSite::new(SourceId::from_raw(0), Span::new(10, 11));
        names.record_ref(site, binding);
        assert_eq!(names.refs.get(&site).copied(), Some(binding));
    }

    #[test]
    fn symbol_slice_is_boxed() {
        let slice: SymbolSlice = vec![Symbol::from_raw(1), Symbol::from_raw(2)].into_boxed_slice();
        assert_eq!(slice.len(), 2);
    }
}

mod failure {}
