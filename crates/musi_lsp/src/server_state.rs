use async_lsp::{ClientSocket, lsp_types::Url};
use musi_basic::{interner::Interner, source::SourceFile};
use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

#[derive(Clone)]
pub struct ServerState {
    pub client: ClientSocket,
    pub interner: Arc<Mutex<Interner>>,
    pub documents: Arc<Mutex<HashMap<Url, Arc<SourceFile>>>>,
}

impl ServerState {
    pub fn new(client: ClientSocket) -> Self {
        Self {
            client,
            interner: Arc::new(Mutex::new(Interner::default())),
            documents: Arc::new(Mutex::new(HashMap::new())),
        }
    }
}
