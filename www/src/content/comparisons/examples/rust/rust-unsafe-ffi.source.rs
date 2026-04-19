extern "C" { fn clock_ms() -> i64; }

let now = unsafe { clock_ms() };
