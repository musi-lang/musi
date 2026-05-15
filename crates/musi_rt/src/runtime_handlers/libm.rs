use musi_native::NativeHost;
use musi_vm::Value;

use super::errors::foreign_rejected;

pub(super) fn register(host: &mut NativeHost) {
    register_unary(host, "@@std@0.1.0/libm.ms::C__acos", f64::acos);
    register_unary(host, "@@std@0.1.0/libm.ms::C__asin", f64::asin);
    register_unary(host, "@@std@0.1.0/libm.ms::C__atan", f64::atan);
    register_binary(host, "@@std@0.1.0/libm.ms::C__atan2", f64::atan2);
    register_unary(host, "@@std@0.1.0/libm.ms::C__ceil", f64::ceil);
    register_unary(host, "@@std@0.1.0/libm.ms::C__cos", f64::cos);
    register_unary(host, "@@std@0.1.0/libm.ms::C__exp", f64::exp);
    register_unary(host, "@@std@0.1.0/libm.ms::C__fabs", f64::abs);
    register_unary(host, "@@std@0.1.0/libm.ms::C__floor", f64::floor);
    register_binary(host, "@@std@0.1.0/libm.ms::C__fmod", fmod);
    register_binary(host, "@@std@0.1.0/libm.ms::C__hypot", f64::hypot);
    register_unary(host, "@@std@0.1.0/libm.ms::C__log", f64::ln);
    register_unary(host, "@@std@0.1.0/libm.ms::C__log10", f64::log10);
    register_binary(host, "@@std@0.1.0/libm.ms::C__pow", f64::powf);
    register_unary(host, "@@std@0.1.0/libm.ms::C__round", f64::round);
    register_unary(host, "@@std@0.1.0/libm.ms::C__sin", f64::sin);
    register_unary(host, "@@std@0.1.0/libm.ms::C__sqrt", f64::sqrt);
    register_unary(host, "@@std@0.1.0/libm.ms::C__tan", f64::tan);
    register_unary(host, "@@std@0.1.0/libm.ms::C__trunc", f64::trunc);
}

fn register_unary(host: &mut NativeHost, name: &'static str, op: fn(f64) -> f64) {
    host.register_foreign_handler(name, move |foreign, args| {
        let [Value::Float(value)] = args else {
            return Err(foreign_rejected(foreign));
        };
        Ok(Value::Float(op(*value)))
    });
}

fn register_binary(host: &mut NativeHost, name: &'static str, op: fn(f64, f64) -> f64) {
    host.register_foreign_handler(name, move |foreign, args| {
        let [Value::Float(left), Value::Float(right)] = args else {
            return Err(foreign_rejected(foreign));
        };
        Ok(Value::Float(op(*left, *right)))
    });
}

fn fmod(left: f64, right: f64) -> f64 {
    left % right
}
