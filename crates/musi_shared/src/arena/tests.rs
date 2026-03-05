// Tests for the parent module — extracted from the inline test block.
    use super::{Arena, Idx, Slice, SliceArena};

    // ── Arena tests ──────────────────────────────────────────────────────────

    #[test]
    fn alloc_and_get_round_trip() {
        let mut arena: Arena<&str> = Arena::new();
        let idx = arena.alloc("hello");
        assert_eq!(*arena.get(idx), "hello");
    }

    #[test]
    fn alloc_multiple_and_get_each() {
        let mut arena = Arena::new();
        let a = arena.alloc(10);
        let b = arena.alloc(20);
        let c = arena.alloc(30);
        assert_eq!(*arena.get(a), 10);
        assert_eq!(*arena.get(b), 20);
        assert_eq!(*arena.get(c), 30);
    }

    #[test]
    fn get_mut_modifies_item() {
        let mut arena = Arena::new();
        let idx = arena.alloc(1);
        *arena.get_mut(idx) = 42;
        assert_eq!(*arena.get(idx), 42);
    }

    #[test]
    fn len_and_is_empty() {
        let mut arena: Arena<i32> = Arena::new();
        assert!(arena.is_empty());
        assert_eq!(arena.len(), 0);

        let _a = arena.alloc(1);
        assert!(!arena.is_empty());
        assert_eq!(arena.len(), 1);

        let _b = arena.alloc(2);
        assert_eq!(arena.len(), 2);
    }

    #[test]
    fn with_capacity_works() {
        let mut arena: Arena<i32> = Arena::with_capacity(64);
        assert!(arena.is_empty());
        let _a = arena.alloc(1);
        assert_eq!(arena.len(), 1);
    }

    #[test]
    fn iter_visits_all_items_in_order() {
        let mut arena = Arena::new();
        let _a = arena.alloc(10u32);
        let _b = arena.alloc(20u32);
        let _c = arena.alloc(30u32);
        let collected: Vec<u32> = arena.iter().copied().collect();
        assert_eq!(collected, vec![10, 20, 30]);
    }

    #[test]
    fn iter_idx_yields_correct_indices() {
        let mut arena: Arena<u32> = Arena::new();
        let a = arena.alloc(100);
        let b = arena.alloc(200);
        let pairs: Vec<(Idx<u32>, u32)> = arena.iter_idx().map(|(i, &v)| (i, v)).collect();
        assert_eq!(pairs[0].0, a);
        assert_eq!(pairs[0].1, 100);
        assert_eq!(pairs[1].0, b);
        assert_eq!(pairs[1].1, 200);
    }

    // `Idx<A>` and `Idx<B>` are distinct types when `A != B`, so the compiler
    // rejects using an `Idx<String>` to index an `Arena<i32>`. This is enforced
    // at compile time and cannot be demonstrated in a runtime test.

    #[test]
    fn idx_is_copy() {
        let mut arena = Arena::new();
        let idx = arena.alloc("value");
        let copy = idx;
        // Both the original and the copy are usable.
        assert_eq!(*arena.get(idx), "value");
        assert_eq!(*arena.get(copy), "value");
    }

    #[test]
    fn idx_debug_format() {
        let idx: Idx<()> = Idx::new(7);
        assert_eq!(format!("{idx:?}"), "Idx(7)");
    }

    #[test]
    fn idx_ordering() {
        let a: Idx<()> = Idx::new(1);
        let b: Idx<()> = Idx::new(2);
        assert!(a < b);
        assert_eq!(a, a);
    }

    #[test]
    fn default_arena_is_empty() {
        let arena: Arena<i32> = Arena::default();
        assert!(arena.is_empty());
    }

    // ── SliceArena tests ─────────────────────────────────────────────────────

    #[test]
    fn slice_empty_is_zero_len() {
        let s: Slice<u32> = Slice::empty();
        assert!(s.is_empty());
        assert_eq!(s.len(), 0);
    }

    #[test]
    fn slice_arena_alloc_slice_round_trip() {
        let mut arena: SliceArena<u32> = SliceArena::new();
        let s = arena.alloc_slice([1, 2, 3]);
        assert_eq!(arena.get_slice(s), &[1, 2, 3]);
    }

    #[test]
    fn slice_arena_two_slices_independent() {
        let mut arena: SliceArena<u32> = SliceArena::new();
        let a = arena.alloc_slice([10, 20]);
        let b = arena.alloc_slice([30, 40, 50]);
        assert_eq!(arena.get_slice(a), &[10, 20]);
        assert_eq!(arena.get_slice(b), &[30, 40, 50]);
    }

    #[test]
    fn slice_empty_in_arena_returns_empty_slice() {
        let arena: SliceArena<u32> = SliceArena::new();
        assert_eq!(arena.get_slice(Slice::empty()), &[] as &[u32]);
    }

    #[test]
    fn slice_arena_get_slice_mut() {
        let mut arena: SliceArena<u32> = SliceArena::new();
        let s = arena.alloc_slice([1, 2, 3]);
        arena.get_slice_mut(s)[1] = 99;
        assert_eq!(arena.get_slice(s), &[1, 99, 3]);
    }

    #[test]
    fn slice_is_copy_and_eq() {
        let mut arena: SliceArena<u32> = SliceArena::new();
        let s = arena.alloc_slice([7, 8]);
        let t = s; // copy
        assert_eq!(s, t);
        assert_eq!(arena.get_slice(s), arena.get_slice(t));
    }

    #[test]
    fn slice_debug_format() {
        let mut arena: SliceArena<u32> = SliceArena::new();
        let _pre = arena.alloc_slice([0u32]); // offset start to 1
        let s = arena.alloc_slice([1u32, 2]);
        // start=1, len=2 → "Slice(1..3)"
        assert_eq!(format!("{s:?}"), "Slice(1..3)");
    }

    #[test]
    fn slice_arena_len_and_is_empty() {
        let mut arena: SliceArena<u32> = SliceArena::new();
        assert!(arena.is_empty());
        let _s = arena.alloc_slice([1, 2, 3]);
        assert_eq!(arena.len(), 3);
        assert!(!arena.is_empty());
    }

    #[test]
    fn slice_arena_with_capacity_works() {
        let mut arena: SliceArena<u32> = SliceArena::with_capacity(16);
        assert!(arena.is_empty());
        let s = arena.alloc_slice([42]);
        assert_eq!(arena.get_slice(s), &[42]);
    }

    #[test]
    fn alloc_empty_slice_returns_empty() {
        let mut arena: SliceArena<u32> = SliceArena::new();
        let s = arena.alloc_slice(core::iter::empty());
        assert!(s.is_empty());
        assert_eq!(arena.get_slice(s), &[] as &[u32]);
    }
