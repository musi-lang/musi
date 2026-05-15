use musi_native::NativeHost;

use crate::output::RuntimeOutputSinkCell;

mod bits;
mod crypto;
mod encoding;
mod env;
mod errors;
mod format;
mod fs;
mod io_log;
mod json;
mod libc;
mod libm;
mod process;
mod sys;
mod text;
mod time_random;
mod uuid;
mod values;

pub fn register_runtime_handlers(host: &mut NativeHost, output: &RuntimeOutputSinkCell) {
    bits::register(host);
    env::register(host);
    process::register(host);
    sys::register(host);
    time_random::register(host);
    io_log::register(host, output);
    fs::register(host);
    text::register(host);
    json::register(host);
    libc::register(host);
    libm::register(host);
    encoding::register(host);
    format::register(host);
    crypto::register(host);
    uuid::register(host);
}
