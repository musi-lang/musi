//! Unbounded FIFO channels for inter-task communication.

use std::collections::VecDeque;

use crate::error::VmError;
use crate::value::Value;

/// An unbounded FIFO channel.
pub struct Channel {
    pub id: u32,
    pub buffer: VecDeque<Value>,
}

/// Registry of all live channels.
pub struct ChannelTable {
    channels: Vec<Channel>,
    next_id: u32,
}

impl ChannelTable {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            channels: vec![],
            next_id: 0,
        }
    }

    /// Create a new channel and return its id.
    pub fn create(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.channels.push(Channel {
            id,
            buffer: VecDeque::new(),
        });
        id
    }

    /// Send a value into a channel.
    pub fn send(&mut self, chan_id: u32, value: Value) -> Result<(), VmError> {
        let chan =
            self.channels
                .iter_mut()
                .find(|c| c.id == chan_id)
                .ok_or(VmError::UnknownChannel {
                    channel_id: chan_id,
                })?;
        chan.buffer.push_back(value);
        Ok(())
    }

    /// Try to receive a value from a channel. Returns `None` if buffer is empty.
    pub fn try_recv(&mut self, chan_id: u32) -> Result<Option<Value>, VmError> {
        let chan =
            self.channels
                .iter_mut()
                .find(|c| c.id == chan_id)
                .ok_or(VmError::UnknownChannel {
                    channel_id: chan_id,
                })?;
        Ok(chan.buffer.pop_front())
    }

    /// Iterate all channels (for GC root collection).
    pub fn channels(&self) -> &[Channel] {
        &self.channels
    }
}
