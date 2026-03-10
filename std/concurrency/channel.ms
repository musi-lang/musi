/// A typed channel for message passing between tasks.
export let Channel ['T] := choice { Channel of 'T };

/// Send a value on the channel.
export let send ['T] : (Channel of 'T, 'T) ~> () under { Async };

/// Receive a value from the channel, blocking until one is available.
export let recv ['T] : (Channel of 'T) ~> 'T under { Async };

/// Try to receive a value without blocking, returning `None` if empty.
export let try_recv ['T] : (Channel of 'T) ~> ?'T under { Async };
