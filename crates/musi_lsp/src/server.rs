use anyhow::Result;
use lsp_server::{Connection, Message};
use lsp_types::{
    InitializeParams, InitializeResult, ServerCapabilities, ServerInfo, TextDocumentSyncCapability,
    TextDocumentSyncKind,
};

use crate::dispatch::{dispatch_notification, dispatch_request};
use crate::state::GlobalState;

pub struct LspServer {
    conn: Connection,
    state: GlobalState,
}

impl LspServer {
    pub fn new(conn: Connection) -> Self {
        let state = GlobalState::new(conn.sender.clone());
        Self { conn, state }
    }

    pub fn run(&mut self) -> Result<()> {
        self.initialize()?;
        self.main_loop()
    }

    fn initialize(&self) -> Result<()> {
        let (init_id, init_params) = self.conn.initialize_start()?;
        let _init_params: InitializeParams = serde_json::from_value(init_params)?;

        let caps = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            ..Default::default()
        };

        let init_result = InitializeResult {
            capabilities: caps,
            server_info: Some(ServerInfo {
                name: "musi-lsp".into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
        };

        self.conn
            .initialize_finish(init_id, serde_json::to_value(init_result)?)?;

        Ok(())
    }

    fn main_loop(&mut self) -> Result<()> {
        loop {
            let Ok(msg) = self.conn.receiver.recv() else {
                return Ok(());
            };

            match msg {
                Message::Request(req) => {
                    if self.conn.handle_shutdown(&req)? {
                        return Ok(());
                    }
                    if let Some(resp) = dispatch_request(&mut self.state, &req) {
                        self.conn.sender.send(Message::Response(resp))?;
                    }
                }
                Message::Notification(notif) => {
                    dispatch_notification(&mut self.state, notif)?;
                }
                Message::Response(_) => {}
            }
        }
    }
}
