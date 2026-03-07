use musi_macros::musi_module;

#[musi_module]
pub mod math {
    pub fn float_sqrt(x: f64) -> f64 { x.sqrt() }
    pub fn float_pow(x: f64, y: f64) -> f64 { x.powf(y) }
    pub fn float_floor(x: f64) -> f64 { x.floor() }
    pub fn float_ceil(x: f64) -> f64  { x.ceil() }
}
