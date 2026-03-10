//! Channel types for inter-task communication.

use std::collections::VecDeque;

use crate::value::Value;

/// A buffered channel for passing values between tasks.
pub struct Channel {
    /// Unique channel identifier.
    pub channel_id: u32,
    /// Buffered values waiting to be received.
    pub buffer: VecDeque<Value>,
    /// Task ids waiting to receive from this channel.
    pub waiters: Vec<u32>,
}

impl Channel {
    /// Create a new empty channel.
    #[must_use]
    pub const fn new(channel_id: u32) -> Self {
        Self {
            channel_id,
            buffer: VecDeque::new(),
            waiters: vec![],
        }
    }
}
