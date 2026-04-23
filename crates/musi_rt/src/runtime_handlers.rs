use musi_native::NativeHost;

use crate::output::RuntimeOutputSinkCell;

mod crypto;
mod encoding;
mod env;
mod errors;
mod format;
mod fs;
mod io_log;
mod json;
mod process;
mod text;
mod time_random;
mod uuid;
mod values;

pub fn register_runtime_handlers(host: &mut NativeHost, output: &RuntimeOutputSinkCell) {
    env::register(host);
    process::register(host);
    time_random::register(host);
    io_log::register(host, output);
    fs::register(host);
    text::register(host);
    json::register(host);
    encoding::register(host);
    format::register(host);
    crypto::register(host);
    uuid::register(host);
}
