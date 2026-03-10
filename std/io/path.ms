/// Join two path segments with the platform separator.
export let join : (String, String) -> String;

/// Return the directory portion of a path.
export let dirname : String -> String;

/// Return the file name portion of a path.
export let basename : String -> String;

/// Return the file extension, or `None` if there is none.
export let extension : String -> ?String;

/// Return the file name without its extension.
export let stem : String -> String;

/// True if the path is absolute.
export let is_absolute : String -> Bool;

/// Return the parent directory, or `None` for root paths.
export let parent : String -> ?String;

/// Normalize a path, resolving `.` and `..` segments.
export let normalize : String -> String;
