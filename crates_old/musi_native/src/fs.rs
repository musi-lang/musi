#![allow(clippy::absolute_paths)]
use musi_macros::musi_module;

#[musi_module]
pub mod fs {
    use std::cell::RefCell;

    pub fn fs_read_file(path: &str) -> Option<String> {
        std::fs::read_to_string(path).ok()
    }

    pub fn fs_write_file(path: &str, content: &str) -> bool {
        std::fs::write(path, content).is_ok()
    }

    pub fn fs_append_file(path: &str, content: &str) -> bool {
        use std::io::Write as _;
        std::fs::OpenOptions::new()
            .append(true)
            .create(true)
            .open(path)
            .and_then(|mut f| f.write_all(content.as_bytes()))
            .is_ok()
    }

    #[musi_src("(path: String): []String")]
    pub fn fs_read_dir(path: &str) -> Value {
        match std::fs::read_dir(path) {
            Ok(entries) => {
                let names: Vec<Value> = entries
                    .filter_map(|e| e.ok())
                    .map(|e| Value::String(Rc::from(e.file_name().to_string_lossy().as_ref())))
                    .collect();
                Value::Array(Rc::new(RefCell::new(names)))
            }
            Err(_) => Value::Array(Rc::new(RefCell::new(Vec::new()))),
        }
    }

    pub fn fs_exists(path: &str) -> bool {
        std::path::Path::new(path).exists()
    }

    pub fn fs_remove(path: &str) -> bool {
        std::fs::remove_file(path)
            .or_else(|_| std::fs::remove_dir_all(path))
            .is_ok()
    }

    #[musi_src("(src: String, dst: String): Bool")]
    pub fn fs_rename(from: &str, to: &str) -> bool {
        std::fs::rename(from, to).is_ok()
    }

    pub fn fs_make_dir(path: &str) -> bool {
        std::fs::create_dir_all(path).is_ok()
    }
}
