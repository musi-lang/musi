/// A handle to an asynchronous computation that will produce a value.
export let Task := forall 'T -> Pending of 'T + Completed of 'T;

/// Spawn a new asynchronous task.
export let spawn : forall 'T -> (() ~> 'T under { Async }) ~> Task of 'T under { Async };

/// Await the result of a task, suspending until it completes.
export let await : forall 'T -> (Task of 'T) ~> 'T under { Async };
