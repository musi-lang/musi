mod api;
mod host;
mod platform;
mod registered;
mod testing;

pub use api::{NativeHost, WeakNativeHost};
pub use api::{NativeTestCaseResult, NativeTestReport};
pub use platform::{NativeAbiCallSupport, NativeAbiTypePosition};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
