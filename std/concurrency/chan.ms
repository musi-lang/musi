/// A typed channel for message passing between tasks.
///
/// # Examples
///
/// ```musi
/// let ch := Chan.Chan(());
/// send(ch, "hello");
/// let msg := recv(ch)
/// ```
export let Chan ['T] := choice { Chan of 'T };

/// Send a value on the channel.
export let send ['T] : (Chan of 'T, 'T) ~> () under { Async };

/// Receive a value from the channel, blocking until one is available.
export let recv ['T] : (Chan of 'T) ~> 'T under { Async };

/// Try to receive a value without blocking.
///
/// Returns `None` if the channel is empty.
export let try_recv ['T] : (Chan of 'T) ~> ?'T under { Async };
