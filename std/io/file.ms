/// Read the entire contents of a file as a string.
///
/// # Examples
///
/// ```musi
/// let contents := read_file("config.toml")
/// ```
export let read_file : String ~> String under { IO };

/// Write a string to a file, replacing its contents.
///
/// Creates the file if it does not exist.
///
/// # Examples
///
/// ```musi
/// write_file("output.txt", "hello")
/// ```
export let write_file : (String, String) ~> () under { IO };

/// Append a string to the end of a file.
///
/// Creates the file if it does not exist.
export let append_file : (String, String) ~> () under { IO };

/// Return true if a file exists at the given path.
export let file_exists : String ~> Bool under { IO };

/// Delete a file at the given path.
export let delete_file : String ~> () under { IO };

/// Read the contents of a file as raw bytes.
export let read_bytes : String ~> List of Int under { IO };

/// Write raw bytes to a file, replacing its contents.
export let write_bytes : (String, List of Int) ~> () under { IO };
