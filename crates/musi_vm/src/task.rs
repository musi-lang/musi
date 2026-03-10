//! Task types for cooperative concurrency.

use crate::value::Value;
use crate::vm::Frame;

/// A concurrent task with its own call stack.
// Used by TSK_SPN/AWT opcodes when scheduler is wired.
#[allow(dead_code)]
pub struct Task {
    /// Unique task identifier.
    pub task_id: u32,
    /// The task's own call stack.
    pub call_stack: Vec<Frame>,
    /// Current execution status.
    pub status: TaskStatus,
    /// Final result value (set when the task completes).
    pub result: Option<Value>,
}

/// The execution status of a task.
// Used by TSK_SPN/AWT opcodes when scheduler is wired.
#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TaskStatus {
    /// Ready to run.
    Runnable,
    /// Currently executing.
    Running,
    /// Waiting on another task or channel.
    Suspended(SuspendReason),
    /// Finished execution.
    Complete,
}

/// Why a task is suspended.
// Used by TSK_SPN/AWT opcodes when scheduler is wired.
#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SuspendReason {
    /// Waiting for the given task to complete.
    AwaitTask(u32),
    /// Waiting for data on the given channel.
    ChannelRecv(u32),
}
