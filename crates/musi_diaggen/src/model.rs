use std::path::PathBuf;

#[derive(Debug)]
pub struct Catalog {
    pub owner: String,
    pub crate_name: Option<String>,
    pub enum_name: Option<String>,
    pub output: Option<PathBuf>,
    pub entries: Vec<Entry>,
    pub maps: Vec<KindMap>,
}

#[derive(Debug)]
pub struct Entry {
    pub kind: String,
    pub code: u16,
    pub message: String,
    pub primary: String,
    pub secondary: Option<String>,
    pub help: Option<String>,
}

#[derive(Debug)]
pub struct KindMap {
    pub name: String,
    pub source_type: String,
    pub source_by_value: bool,
    pub mode: MapMode,
    pub cases: Vec<MapCase>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MapMode {
    Required,
    Option,
}

#[derive(Debug)]
pub struct MapCase {
    pub pattern: String,
    pub target: MapTarget,
}

#[derive(Debug, PartialEq, Eq)]
pub enum MapTarget {
    Kind(String),
    None,
}
