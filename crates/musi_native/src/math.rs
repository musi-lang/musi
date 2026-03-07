#![allow(clippy::absolute_paths)]
use musi_macros::musi_module;

#[musi_module]
pub mod math {
    pub fn sqrt(n: f64) -> f64 {
        n.sqrt()
    }
    pub fn pow(base: f64, exp: f64) -> f64 {
        base.powf(exp)
    }
    pub fn floor(n: f64) -> f64 {
        n.floor()
    }
    pub fn ceil(n: f64) -> f64 {
        n.ceil()
    }
    pub fn round(n: f64) -> f64 {
        n.round()
    }
    pub fn fabs(n: f64) -> f64 {
        n.abs()
    }
    pub fn fmin(a: f64, b: f64) -> f64 {
        a.min(b)
    }
    pub fn fmax(a: f64, b: f64) -> f64 {
        a.max(b)
    }
    pub fn fclamp(n: f64, lo: f64, hi: f64) -> f64 {
        n.clamp(lo, hi)
    }
}
