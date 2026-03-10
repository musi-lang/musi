//! Task types for cooperative concurrency.

use crate::value::Value;
use crate::vm::Frame;

#[allow(dead_code)]
pub struct Task {
    pub task_id: u32,
    pub call_stack: Vec<Frame>,
    pub status: TaskStatus,
    pub result: Option<Value>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TaskStatus {
    Runnable,
    Running,
    Suspended(SuspendReason),
    Complete,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SuspendReason {
    AwaitTask(u32),
    ChannelRecv(u32),
}
