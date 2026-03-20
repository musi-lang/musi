use super::cache_key;

#[test]
fn test_cache_key_deterministic() {
    let a = cache_key("github.com/user/repo");
    let b = cache_key("github.com/user/repo");
    assert_eq!(a, b);
}

#[test]
fn test_cache_key_different_urls() {
    let a = cache_key("github.com/user/repo-a");
    let b = cache_key("github.com/user/repo-b");
    assert_ne!(a, b);
}

#[test]
fn test_cache_key_length() {
    let key = cache_key("github.com/user/repo");
    assert_eq!(key.len(), 16);
}
