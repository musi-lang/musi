/// Join two path segments with the platform separator.
///
/// # Examples
///
/// ```musi
/// let p := join("/usr", "bin");  -- "/usr/bin"
/// ```
export let join : (String, String) -> String;

/// Return the directory portion of a path.
///
/// # Examples
///
/// ```musi
/// dirname("/usr/bin/env")  -- "/usr/bin"
/// ```
export let dirname : String -> String;

/// Return the file name portion of a path.
///
/// # Examples
///
/// ```musi
/// basename("/usr/bin/env")  -- "env"
/// ```
export let basename : String -> String;

/// Return the file extension, or `None` if there is none.
///
/// # Examples
///
/// ```musi
/// extension("main.ms")  -- .Some("ms")
/// extension("Makefile")  -- .None
/// ```
export let extension : String -> ?String;

/// Return the file name without its extension.
export let stem : String -> String;

/// Return true if the path is absolute.
export let is_absolute : String -> Bool;

/// Return the parent directory, or `None` for root paths.
export let parent : String -> ?String;

/// Normalize a path, resolving `.` and `..` segments.
export let normalize : String -> String;
