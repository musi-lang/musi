// std/path.ms — path manipulation utilities
// import { join, dirname, basename, extension, is_absolute, normalize, stem,
//          path_components, path_with_extension } from "std/path"
// Low-level ops: import { path_join, ... } from "musi:path"

import { path_join, path_dirname, path_basename, path_extension,
         path_is_absolute, path_normalize, path_stem } from "musi:path";

/// Join two path segments, inserting a `/` separator if needed.
export fn join(base: String, part: String): String => path_join(base, part);

/// Return the parent directory, or `.None` for bare filenames and filesystem roots.
export fn dirname(p: String): Option[String] => path_dirname(p);

/// Return the final path component (including extension), or `.None` for bare roots.
export fn basename(p: String): Option[String] => path_basename(p);

/// Return the file extension without the leading dot, or `.None` if there is none.
export fn extension(p: String): Option[String] => path_extension(p);

/// Return `.True` when the path starts with `"/"`.
export fn is_absolute(p: String): Bool => path_is_absolute(p);

/// Resolve `"."` and `".."` segments lexically without accessing the filesystem.
export fn normalize(p: String): String => path_normalize(p);

/// Return the final component without its extension, or `.None` for bare roots.
export fn stem(p: String): Option[String] => path_stem(p);

/// Split a path into non-empty components, stripping leading and trailing slashes.
export fn path_components(p: String): []String => (
    const parts := string_split(p, "/");
    var result := [];
    for s in parts loop (if string_length(s) > 0 then array_push(result, s));
    result
);

/// Return the path with its extension replaced by `ext` (or added if the path has none).
export fn path_with_extension(p: String, ext: String): String => (
    const s := match path_stem(p) with (.Some(x) => x | .None => p);
    const name := string_concat(s, string_concat(".", ext));
    match path_dirname(p) with (
        .None => name
        | .Some(d) => if string_length(d) = 0 then name else path_join(d, name)
    )
);

/// Return `.True` when `child` begins with `parent` (both paths should be normalised first).
export fn path_is_child(parent: String, child: String): Bool => (
    const ps := path_normalize(parent);
    const cs := path_normalize(child);
    const plen := string_length(ps);
    const clen := string_length(cs);
    if clen < plen then .False
    else string_contains(string_slice(cs, 0, plen), ps)
);
