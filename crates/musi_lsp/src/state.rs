use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crossbeam_channel::Sender;
use lsp_server::{Message, Notification};
use lsp_types::Uri;
use lsp_types::notification::Notification as LspNotification;
use musi_basic::{interner::Interner, source::SourceFile};

pub struct GlobalState {
    pub interner: Arc<Mutex<Interner>>,
    pub documents: HashMap<Uri, Arc<SourceFile>>,
    pub sender: Sender<Message>,
}

impl GlobalState {
    pub fn new(sender: Sender<Message>) -> Self {
        Self {
            interner: Arc::new(Mutex::new(Interner::default())),
            documents: HashMap::new(),
            sender,
        }
    }

    pub fn send_notification<N: LspNotification>(&self, params: N::Params) -> anyhow::Result<()> {
        let notif = Notification::new(N::METHOD.into(), serde_json::to_value(params)?);
        self.sender.send(Message::Notification(notif))?;
        Ok(())
    }
}
