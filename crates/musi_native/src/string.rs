#![allow(clippy::absolute_paths)]
use musi_macros::musi_module;

#[musi_module]
pub mod string {
    pub fn string_length(s: &str) -> i64 {
        i64::try_from(s.chars().count()).unwrap_or(i64::MAX)
    }

    pub fn string_slice(s: &str, start: i64, end: i64) -> String {
        let chars: Vec<char> = s.chars().collect();
        let len = i64::try_from(chars.len()).unwrap_or(i64::MAX);
        let lo = usize::try_from(start.clamp(0, len)).unwrap_or(0);
        let hi = usize::try_from(end.clamp(0, len)).unwrap_or(0).max(lo);
        chars[lo..hi].iter().collect()
    }

    pub fn string_to_int(s: &str) -> Option<i64> {
        s.parse().ok()
    }

    pub fn string_contains(s: &str, sub: &str) -> bool {
        s.contains(sub)
    }

    pub fn nat_to_string(n: i64) -> String {
        n.max(0).to_string()
    }
}
