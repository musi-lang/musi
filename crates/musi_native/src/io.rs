#![allow(clippy::absolute_paths)]
use musi_macros::musi_module;

#[musi_module]
pub mod io {
    pub fn read_line() -> String {
        use std::io::BufRead as _;
        let mut line = String::new();
        let _bytes = std::io::stdin().lock().read_line(&mut line).unwrap_or_default();
        if line.ends_with('\n') {
            let _ = line.pop();
            if line.ends_with('\r') { let _ = line.pop(); }
        }
        line
    }
}
