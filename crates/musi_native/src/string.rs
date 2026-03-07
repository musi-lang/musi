use musi_macros::musi_module;

#[musi_module]
pub mod string {
    pub fn string_length(s: &str) -> i64 {
        i64::try_from(s.chars().count()).expect("string length fits i64")
    }

    pub fn string_concat(a: &str, b: &str) -> String {
        let mut out = String::with_capacity(a.len() + b.len());
        out.push_str(a);
        out.push_str(b);
        out
    }

    pub fn string_slice(s: &str, start: i64, end: i64) -> String {
        let len = i64::try_from(s.chars().count()).expect("string length fits i64");
        let (lo, hi) = crate::registry::slice_range(start, end, len);
        s.chars().skip(lo).take(hi - lo).collect()
    }

    pub fn string_to_int(s: &str) -> Option<i64> {
        s.parse::<i64>().ok()
    }

    pub fn string_contains(s: &str, sub: &str) -> bool {
        s.contains(sub)
    }
}
