/// Write a string to standard output.
///
/// # Examples
///
/// ```musi
/// write("hello, world!")
/// ```
export let write : String ~> () under { IO };

/// Write a string followed by a newline to standard output.
///
/// # Examples
///
/// ```musi
/// writeln("hello, world!")
/// ```
export let writeln : String ~> () under { IO };

/// Write a string to standard error.
export let ewrite : String ~> () under { IO };

/// Write a string followed by a newline to standard error.
export let ewriteln : String ~> () under { IO };
