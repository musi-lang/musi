// std/encoding/json.ms — basic JSON serialisation (stringify only)
//
// import { json_null, json_bool, json_int, json_float, json_string,
//          json_array, json_object, json_field } from "std/encoding/json"

import { string_replace } from "std/string";

// JSON null literal.
export fn json_null(): String => "null";

// Boolean to JSON ("true" / "false").
export fn json_bool(b: Bool): String =>
    match b with ( .True => "true" | .False => "false" );

// Integer to JSON.
export fn json_int(n: Int): String => int_to_string(n);

// Float to JSON.
export fn json_float(f: Float): String => float_to_string(f);

// Escape a raw string and wrap in double quotes.
export fn json_string(s: String): String => (
    var e := string_replace(s,    "\\", "\\\\");
    e <- string_replace(e, "\"", "\\\"");
    e <- string_replace(e, "\n",  "\\n");
    e <- string_replace(e, "\r",  "\\r");
    e <- string_replace(e, "\t",  "\\t");
    string_concat("\"", string_concat(e, "\""))
);

// Serialise an array of already-serialised JSON values.
//   json_array([json_int(1), json_int(2)]) == "[1,2]"
export fn json_array(items: []String): String => (
    const n := array_length(items);
    if n = 0 then "[]"
    else (
        var result := "[";
        var i := 0;
        while i < n loop (
            if i > 0 then result <- string_concat(result, ",");
            result <- string_concat(result, array_get(items, i));
            i <- i + 1
        );
        string_concat(result, "]")
    )
);

// Build one JSON object field string: "key":value.
//   json_field("x", json_int(1)) == "\"x\":1"
export fn json_field(key: String, value: String): String =>
    string_concat(json_string(key), string_concat(":", value));

// Serialise an object from already-built field strings.
//   json_object([json_field("a", json_int(1))]) == "{\"a\":1}"
export fn json_object(fields: []String): String => (
    const n := array_length(fields);
    if n = 0 then "{}"
    else (
        var result := "{";
        var i := 0;
        while i < n loop (
            if i > 0 then result <- string_concat(result, ",");
            result <- string_concat(result, array_get(fields, i));
            i <- i + 1
        );
        string_concat(result, "}")
    )
);
