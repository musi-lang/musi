// std/path.ms — path manipulation utilities
// import { join, dirname, basename, extension, is_absolute, normalize, stem,
//          path_components, path_with_extension } from "std/path"
// Low-level ops: import { path_join, ... } from "musi:path"

import { path_join, path_dirname, path_basename, path_extension,
         path_is_absolute, path_normalize, path_stem } from "musi:path";

// Join two path components.
export fn join(base: String, part: String): String => path_join(base, part);

// Parent directory, or None for bare names and filesystem roots.
export fn dirname(p: String): Option[String] => path_dirname(p);

// Final component (including extension), or None for bare roots.
export fn basename(p: String): Option[String] => path_basename(p);

// File extension without the leading dot, or None if absent.
export fn extension(p: String): Option[String] => path_extension(p);

// True when the path starts with "/".
export fn is_absolute(p: String): Bool => path_is_absolute(p);

// Resolve "." and ".." segments without touching the filesystem.
export fn normalize(p: String): String => path_normalize(p);

// Final component without extension, or None for bare roots.
export fn stem(p: String): Option[String] => path_stem(p);

// Split a path into non-empty components (strips leading/trailing slashes).
export fn path_components(p: String): []String => (
    const parts := string_split(p, "/");
    var result := [];
    for s in parts loop (if string_length(s) > 0 then array_push(result, s));
    result
);

// Return path with its extension replaced (or added if none exists).
export fn path_with_extension(p: String, ext: String): String => (
    const s := match path_stem(p) with (.Some(x) => x | .None => p);
    const name := string_concat(s, string_concat(".", ext));
    match path_dirname(p) with (
        .None => name
        | .Some(d) => if string_length(d) = 0 then name else path_join(d, name)
    )
);

// True when child begins with parent (both paths should be normalised first).
export fn path_is_child(parent: String, child: String): Bool => (
    const ps := path_normalize(parent);
    const cs := path_normalize(child);
    const plen := string_length(ps);
    const clen := string_length(cs);
    if clen < plen then .False
    else string_contains(string_slice(cs, 0, plen), ps)
);
