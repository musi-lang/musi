#![allow(clippy::absolute_paths)]
use musi_macros::musi_module;

#[musi_module]
pub mod process {
    use std::cell::RefCell;

    #[musi_src("(): []String")]
    pub fn process_args() -> Value {
        let items: Vec<Value> = std::env::args()
            .map(|s| Value::String(Rc::from(s.as_str())))
            .collect();
        Value::Array(Rc::new(RefCell::new(items)))
    }

    pub fn process_env_get(key: &str) -> Option<String> {
        std::env::var(key).ok()
    }

    pub fn process_exit(code: i64) {
        std::process::exit(i32::try_from(code).unwrap_or(1));
    }

    pub fn process_cwd() -> Option<String> {
        std::env::current_dir()
            .ok()
            .map(|p| p.to_string_lossy().into_owned())
    }
}
