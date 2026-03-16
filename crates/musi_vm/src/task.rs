//! Cooperative green-thread scheduler for Musi concurrency.

use std::collections::VecDeque;

use crate::value::Value;
use crate::vm::{Continuation, Frame};

/// The execution status of a task.
#[derive(Debug, Clone)]
pub enum TaskStatus {
    Ready,
    Running,
    AwaitingTask { task_id: u32 },
    AwaitingRecv { chan_id: u32 },
    Completed(Value),
}

/// A cooperative green thread.
pub struct Task {
    pub id: u32,
    pub call_stack: Vec<Frame>,
    pub continuations: Vec<Continuation>,
    pub status: TaskStatus,
}

/// Round-robin task scheduler.
pub struct TaskScheduler {
    tasks: Vec<Task>,
    ready_queue: VecDeque<u32>,
    current_task_id: Option<u32>,
    next_task_id: u32,
}

impl TaskScheduler {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            tasks: vec![],
            ready_queue: VecDeque::new(),
            current_task_id: None,
            next_task_id: 0,
        }
    }

    /// Promote the main execution context to task 0.
    pub fn init_main_task(
        &mut self,
        call_stack: Vec<Frame>,
        continuations: Vec<Continuation>,
    ) -> u32 {
        let id = self.alloc_id();
        self.tasks.push(Task {
            id,
            call_stack,
            continuations,
            status: TaskStatus::Running,
        });
        self.current_task_id = Some(id);
        id
    }

    /// Spawn a new task that will execute starting from the given call frame.
    pub fn spawn(&mut self, frame: Frame) -> u32 {
        let id = self.alloc_id();
        self.tasks.push(Task {
            id,
            call_stack: vec![frame],
            continuations: vec![],
            status: TaskStatus::Ready,
        });
        self.ready_queue.push_back(id);
        id
    }

    /// Look up a task by id.
    pub fn get(&self, task_id: u32) -> Option<&Task> {
        self.tasks.iter().find(|t| t.id == task_id)
    }

    /// Look up a task mutably by id.
    pub fn get_mut(&mut self, task_id: u32) -> Option<&mut Task> {
        self.tasks.iter_mut().find(|t| t.id == task_id)
    }

    /// The currently running task id.
    #[must_use]
    pub const fn current_task_id(&self) -> Option<u32> {
        self.current_task_id
    }

    /// Mark the current task as completed with the given value, then pick next.
    pub fn complete_current(&mut self, value: Value) -> Option<u32> {
        if let Some(cur_id) = self.current_task_id
            && let Some(task) = self.get_mut(cur_id)
        {
            task.status = TaskStatus::Completed(value);
            task.call_stack.clear();
            task.continuations.clear();
        }
        self.wake_awaiters_for(self.current_task_id, value);
        self.schedule_next()
    }

    /// Suspend the current task because it is awaiting another task.
    pub fn suspend_awaiting_task(&mut self, awaited_task_id: u32) {
        if let Some(cur_id) = self.current_task_id
            && let Some(task) = self.get_mut(cur_id)
        {
            task.status = TaskStatus::AwaitingTask {
                task_id: awaited_task_id,
            };
        }
    }

    /// Suspend the current task because it is awaiting a channel recv.
    pub fn suspend_awaiting_recv(&mut self, chan_id: u32) {
        if let Some(cur_id) = self.current_task_id
            && let Some(task) = self.get_mut(cur_id)
        {
            task.status = TaskStatus::AwaitingRecv { chan_id };
        }
    }

    /// Pick the next ready task and set it as current. Returns its id.
    pub fn schedule_next(&mut self) -> Option<u32> {
        let next_id = self.ready_queue.pop_front()?;
        if let Some(task) = self.get_mut(next_id) {
            task.status = TaskStatus::Running;
        }
        self.current_task_id = Some(next_id);
        Some(next_id)
    }

    /// Wake all tasks awaiting the completed task id.
    fn wake_awaiters_for(&mut self, completed_id: Option<u32>, value: Value) {
        let Some(completed_id) = completed_id else {
            return;
        };
        for task in &mut self.tasks {
            if let TaskStatus::AwaitingTask { task_id } = task.status
                && task_id == completed_id
            {
                task.status = TaskStatus::Ready;
                if let Some(frame) = task.call_stack.last_mut() {
                    frame.stack.push(value);
                }
                self.ready_queue.push_back(task.id);
            }
        }
    }

    /// Wake one task waiting on a channel recv, pushing the value onto its stack.
    pub fn wake_one_receiver(&mut self, chan_id: u32, value: Value) -> bool {
        for task in &mut self.tasks {
            if let TaskStatus::AwaitingRecv {
                chan_id: waiting_chan,
            } = task.status
                && waiting_chan == chan_id
            {
                task.status = TaskStatus::Ready;
                if let Some(frame) = task.call_stack.last_mut() {
                    frame.stack.push(value);
                }
                self.ready_queue.push_back(task.id);
                return true;
            }
        }
        false
    }

    /// Check if any non-completed tasks remain (for deadlock detection).
    #[must_use]
    pub fn has_live_tasks(&self) -> bool {
        self.tasks
            .iter()
            .any(|t| !matches!(t.status, TaskStatus::Completed(_)))
    }

    /// Iterate all tasks (for GC root collection).
    pub fn tasks(&self) -> &[Task] {
        &self.tasks
    }

    const fn alloc_id(&mut self) -> u32 {
        let id = self.next_task_id;
        self.next_task_id += 1;
        id
    }
}
