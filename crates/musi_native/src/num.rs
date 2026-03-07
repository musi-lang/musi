use musi_macros::musi_module;

#[musi_module]
pub mod num {
    pub fn int_to_string(n: i64) -> String   { n.to_string() }
    pub fn float_to_string(n: f64) -> String { n.to_string() }
    pub fn nat_to_string(n: i64) -> String   { n.to_string() }
}
