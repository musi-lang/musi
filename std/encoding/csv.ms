// std/encoding/csv.ms — simple CSV parse and serialize
// import { csv_parse_line, csv_parse, csv_serialize_row, csv_serialize } from "std/encoding/csv"

import { string_join, string_lines, string_is_empty } from "std/string";

/// Split a single CSV line on commas and return the fields.
export fn csv_parse_line(line: String): []String => string_split(line, ",");

/// Parse a CSV string into rows of fields, skipping blank lines.
export fn csv_parse(content: String): [][]String => (
    var result := [];
    for line in string_lines(content) loop (
        if not string_is_empty(string_trim(line))
        then array_push(result, csv_parse_line(line))
    );
    result
);

/// Serialise a single row of fields to a comma-separated line.
export fn csv_serialize_row(row: []String): String => string_join(row, ",");

/// Serialise a 2D array of fields to a CSV string (rows joined by newlines).
export fn csv_serialize(rows: [][]String): String => (
    var lines := [];
    for row in rows loop (array_push(lines, csv_serialize_row(row)));
    string_join(lines, "\n")
);
