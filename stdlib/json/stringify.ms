//! JSON serializer.

import "@std/rt" as rt;
import "./types" as _;

let CH_QUOTE := 34;
let CH_BACKSLASH := 92;
let CH_LF := 10;
let CH_CR := 13;
let CH_TAB := 9;

let escape_str := (s) => (
  let chars := rt.str_chars(s);
  let len := rt.arr_len(chars);
  let mut buf := [];
  let step := (i) => (
    () if i >= len
    | (
      let c := rt.rune_to_int(chars.[i]);
      (rt.arr_push(buf, '\\'); rt.arr_push(buf, '"');  step(i + 1)) if c = CH_QUOTE
      | (rt.arr_push(buf, '\\'); rt.arr_push(buf, '\\'); step(i + 1)) if c = CH_BACKSLASH
      | (rt.arr_push(buf, '\\'); rt.arr_push(buf, 'n');  step(i + 1)) if c = CH_LF
      | (rt.arr_push(buf, '\\'); rt.arr_push(buf, 'r');  step(i + 1)) if c = CH_CR
      | (rt.arr_push(buf, '\\'); rt.arr_push(buf, 't');  step(i + 1)) if c = CH_TAB
      | (rt.arr_push(buf, chars.[i]); step(i + 1)) if _
    ) if _
  );
  step(0);
  rt.str_from_chars(buf)
);

/// Serializes a JsonValue to a compact JSON string.
export let stringify := (value : JsonValue) => (
  match value (
    .Null        => "null"
    | .Bool(b)   => ("true" if b | "false" if _)
    | .Number(n) => rt.show(n)
    | .Str(s)    => f"\"{escape_str(s)}\""
    | .Array(arr) => (
      let len := rt.arr_len(arr);
      let mut parts := [];
      let step := (i) => (
        () if i >= len
        | (rt.arr_push(parts, stringify(arr.[i])); step(i + 1)) if _
      );
      step(0);
      f"[{rt.str_join(parts, ", ")}]"
    )
    | .Object(pairs) => (
      let len := rt.arr_len(pairs);
      let mut parts := [];
      let step := (i) => (
        () if i >= len
        | (
          let pair : (String, JsonValue) := pairs.[i];
          rt.arr_push(parts, f"\"{escape_str(pair.0)}\": {stringify(pair.1)}");
          step(i + 1)
        ) if _
      );
      step(0);
      // String concatenation used here because f-strings cannot yet escape literal braces.
      "{" + rt.str_join(parts, ", ") + "}"
    )
  )
);
