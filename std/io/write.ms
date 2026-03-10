/// Write a string to standard output.
export let write : String ~> () under { IO };

/// Write a string followed by a newline to standard output.
export let writeln : String ~> () under { IO };

/// Write a string to standard error.
export let ewrite : String ~> () under { IO };

/// Write a string followed by a newline to standard error.
export let ewriteln : String ~> () under { IO };
