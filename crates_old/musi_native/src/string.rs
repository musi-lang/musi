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

    /// Convert a Rune (Unicode code point) to a single-character String.
    #[musi_src("(r: Rune): String")]
    pub fn rune_to_string(r: i64) -> String {
        u32::try_from(r)
            .ok()
            .and_then(char::from_u32)
            .map_or_else(String::new, |c| c.to_string())
    }

    /// Iterate over a String's Unicode code points, returning them as []Rune.
    #[musi_src("(s: String): []Rune")]
    pub fn string_to_runes(s: &str) -> Value {
        let runes: Vec<Value> = s
            .chars()
            .map(|c| Value::Int(i64::from(u32::from(c))))
            .collect();
        Value::Array(Rc::new(std::cell::RefCell::new(runes)))
    }

    /// Get the Unicode code point of a Rune as an Int.
    #[musi_src("(r: Rune): Int")]
    pub fn rune_to_int(r: i64) -> i64 {
        r
    }

    /// Convert an Int (Unicode code point) to a Rune.
    #[musi_src("(n: Int): Rune")]
    pub fn int_to_rune(n: i64) -> i64 {
        n
    }
}
