use super::{SliceArena, SliceRange};

#[test]
fn slice_range_reports_len() {
    let range = SliceRange::<u8>::new(2, 5);
    assert_eq!(range.len(), 3);
}

#[test]
fn slice_arena_allocates_empty_and_non_empty_slices() {
    let mut arena = SliceArena::new();
    let empty = arena.alloc_from_iter(Vec::<u8>::new());
    let filled = arena.alloc_from_iter([1_u8, 2, 3]);

    assert!(empty.is_empty());
    assert_eq!(arena.get(empty), &[]);
    assert_eq!(arena.get(filled), &[1, 2, 3]);
}

#[test]
fn slice_arena_allocates_from_slice() {
    let mut arena = SliceArena::new();
    let range = arena.alloc_from_slice(&["a", "b"]);
    assert_eq!(arena.get(range), &["a", "b"]);
}
