// std/encoding/csv.ms — simple CSV parse and serialize
// import { csv_parse_line, csv_parse, csv_serialize_row, csv_serialize } from "std/encoding/csv"

import { string_join, string_lines, string_is_empty } from "std/string";

export fn csv_parse_line(line: String): []String => string_split(line, ",");

export fn csv_parse(content: String): [][]String => (
    var result := [];
    for line in string_lines(content) loop (
        if not string_is_empty(string_trim(line))
        then array_push(result, csv_parse_line(line))
    );
    result
);

export fn csv_serialize_row(row: []String): String => string_join(row, ",");

export fn csv_serialize(rows: [][]String): String => (
    var lines := [];
    for row in rows loop (array_push(lines, csv_serialize_row(row)));
    string_join(lines, "\n")
);
