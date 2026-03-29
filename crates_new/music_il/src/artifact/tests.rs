use super::*;

#[test]
fn test_constant_pool_deduplicates_entries() {
    let mut pool = ConstantPool::new();

    let first = pool.add(ConstantEntry::Int(42));
    let second = pool.add(ConstantEntry::Int(42));
    let third = pool.add(ConstantEntry::Tag(7));

    assert_eq!(first, second);
    assert_ne!(first, third);
    assert_eq!(pool.entries().len(), 2);
}
