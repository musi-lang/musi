// std/io.ms — filesystem I/O
// import { io_read, io_write, io_append, io_read_dir, io_exists,
//          io_remove, io_rename, io_make_dir } from "std/io"
// Low-level ops: import { fs_read_file, ... } from "musi:fs"

import { fs_read_file, fs_write_file, fs_append_file, fs_read_dir,
         fs_exists, fs_remove, fs_rename, fs_make_dir } from "musi:fs";

// Read entire file to string. Returns None if file is missing or unreadable.
export fn io_read(path: String): Option[String] => fs_read_file(path);

// Write content to file, replacing it. Returns True on success.
export fn io_write(path: String, content: String): Bool => fs_write_file(path, content);

// Append content to file (creates file if missing). Returns True on success.
export fn io_append(path: String, content: String): Bool => fs_append_file(path, content);

// List directory entries as an array of names. Returns empty array on failure.
export fn io_read_dir(path: String): []String => fs_read_dir(path);

// Check whether a path exists on the filesystem.
export fn io_exists(path: String): Bool => fs_exists(path);

// Remove a file or directory tree. Returns True on success.
export fn io_remove(path: String): Bool => fs_remove(path);

// Rename or move a path. Returns True on success.
export fn io_rename(src: String, dst: String): Bool => fs_rename(src, dst);

// Create a directory and any missing parents. Returns True on success.
export fn io_make_dir(path: String): Bool => fs_make_dir(path);

// Read lines from a file. Returns empty array if file is missing.
export fn io_read_lines(path: String): []String => (
    match io_read(path) with (
        .None => []
        | .Some(content) => string_split(content, "\n")
    )
);

// Write a sequence of lines to a file (joined by newline). Returns True on success.
export fn io_write_lines(path: String, lines: []String): Bool => (
    const n := array_length(lines);
    if n = 0 then io_write(path, "")
    else (
        var result := array_get(lines, 0);
        var i := 1;
        while i < n loop (
            result <- string_concat(result, string_concat("\n", array_get(lines, i)));
            i <- i + 1
        );
        io_write(path, result)
    )
);
