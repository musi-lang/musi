//! Channel types for inter-task communication.

use std::collections::VecDeque;

use crate::value::Value;

#[allow(dead_code)]
pub struct Channel {
    pub channel_id: u32,
    pub buffer: VecDeque<Value>,
    pub waiters: Vec<u32>,
}

impl Channel {
    #[allow(dead_code)]
    #[must_use]
    pub const fn new(channel_id: u32) -> Self {
        Self {
            channel_id,
            buffer: VecDeque::new(),
            waiters: vec![],
        }
    }
}
