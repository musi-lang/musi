/// Read the entire contents of a file as a string.
export let read_file : String ~> String under { IO };

/// Write a string to a file, replacing its contents.
export let write_file : (String, String) ~> () under { IO };

/// Append a string to the end of a file.
export let append_file : (String, String) ~> () under { IO };

/// True if the file exists at the given path.
export let file_exists : String ~> Bool under { IO };

/// Delete a file at the given path.
export let delete_file : String ~> () under { IO };

/// Read the contents of a file as raw bytes.
export let read_bytes : String ~> List of Int under { IO };

/// Write raw bytes to a file, replacing its contents.
export let write_bytes : (String, List of Int) ~> () under { IO };
