// std/collections/queue.ms — FIFO queue backed by []'T
//
// queue_push  is amortised O(1)  (array_push to the back).
// queue_pop   is O(n)            (returns a new slice; original is unchanged).
// For high-throughput queues, prefer a deque built on two arrays.
//
// import { queue_new, queue_push, queue_pop, queue_peek,
//          queue_len, queue_is_empty } from "std/collections/queue"

/// Create an empty queue.
export fn queue_new['T](): []'T => [];

/// Enqueue `val` at the back (mutates `q` in place; amortised O(1)).
export fn queue_push['T](q: []'T, val: 'T): Unit => array_push(q, val);

/// Return the number of elements waiting in the queue.
export fn queue_len['T](q: []'T): Int => array_length(q);

/// Return `.True` when the queue contains no elements.
export fn queue_is_empty['T](q: []'T): Bool => array_length(q) = 0;

/// Return the front element without removing it, or `.None` if empty.
export fn queue_peek['T](q: []'T): Option['T] =>
    if array_length(q) = 0 then .None else .Some(array_get(q, 0));

/// Remove and return the front element together with the remaining queue (O(n)).
/// Returns `(value, remaining_queue)` — the original queue is not modified.
/// Example: `const (v, q2) := queue_pop(q);`
export fn queue_pop['T](q: []'T): (Option['T], []'T) =>
    if array_length(q) = 0
    then (.None, [])
    else (.Some(array_get(q, 0)), array_slice(q, 1, array_length(q)));

/// Return all elements as an array in FIFO order.
export fn queue_to_array['T](q: []'T): []'T => array_slice(q, 0, array_length(q));
