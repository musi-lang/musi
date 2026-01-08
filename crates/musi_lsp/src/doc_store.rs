use std::collections::HashMap;

use tower_lsp_server::ls_types::Uri;

#[derive(Debug)]
pub struct DocumentStore {
    documents: HashMap<Uri, String>,
}

impl DocumentStore {
    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
        }
    }

    pub fn open(&mut self, uri: Uri, text: String) {
        let _ = self.documents.insert(uri, text);
    }

    pub fn close(&mut self, uri: &Uri) {
        let _ = self.documents.remove(uri);
    }

    pub fn update(&mut self, uri: &Uri, text: String) {
        if let Some(doc) = self.documents.get_mut(uri) {
            *doc = text;
        }
    }

    pub fn get(&self, uri: &Uri) -> Option<&String> {
        self.documents.get(uri)
    }
}
