use musi_macros::musi_module;

#[musi_module]
pub mod path {
    pub fn path_join(base: &str, part: &str) -> String {
        std::path::Path::new(base)
            .join(part)
            .to_string_lossy()
            .into_owned()
    }

    pub fn path_dirname(p: &str) -> Option<String> {
        std::path::Path::new(p)
            .parent()
            .map(|d| d.to_string_lossy().into_owned())
    }

    pub fn path_basename(p: &str) -> Option<String> {
        std::path::Path::new(p)
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
    }

    pub fn path_extension(p: &str) -> Option<String> {
        std::path::Path::new(p)
            .extension()
            .map(|e| e.to_string_lossy().into_owned())
    }

    pub fn path_is_absolute(p: &str) -> bool {
        std::path::Path::new(p).is_absolute()
    }

    pub fn path_normalize(p: &str) -> String {
        let mut parts: Vec<&str> = Vec::new();
        let is_absolute = p.starts_with('/');
        for part in p.split('/') {
            match part {
                "" | "." => {}
                ".."     => { let _ = parts.pop(); }
                other    => parts.push(other),
            }
        }
        let joined = parts.join("/");
        if is_absolute {
            format!("/{joined}")
        } else if joined.is_empty() {
            String::from(".")
        } else {
            joined
        }
    }

    pub fn path_stem(p: &str) -> Option<String> {
        std::path::Path::new(p)
            .file_stem()
            .map(|s| s.to_string_lossy().into_owned())
    }
}
