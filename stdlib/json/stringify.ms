//! JSON serializer.

import "@std/rt" as rt;

let escape_str := (s) => (
  let chars := rt.str_chars(s);
  let len := rt.arr_len(chars);
  let step := (i, acc) => (
    acc if i >= len
    | (
      let c := rt.rune_to_int(chars.[i]);
      step(i + 1, acc + "\\\"") if c = 34
      | step(i + 1, acc + "\\\\") if c = 92
      | step(i + 1, acc + "\\n")  if c = 10
      | step(i + 1, acc + "\\r")  if c = 13
      | step(i + 1, acc + "\\t")  if c = 9
      | step(i + 1, acc + rt.str_from_chars([chars.[i]])) if _
    ) if _
  );
  step(0, "")
);

export let stringify := (value) => (
  match value (
    .Null        => "null"
    | .Bool(b)   => ("true" if b | "false" if _)
    | .Number(n) => rt.show(n)
    | .Str(s)    => "\"" + escape_str(s) + "\""
    | .Array(arr) => (
      let len := rt.arr_len(arr);
      let step := (i, acc) => (
        acc if i >= len
        | step(i + 1, acc + (", " if i > 0 | "" if _) + stringify(arr.[i])) if _
      );
      "[" + step(0, "") + "]"
    )
    | .Object(pairs) => (
      let len := rt.arr_len(pairs);
      let step := (i, acc) => (
        acc if i >= len
        | (
          let pair := pairs.[i];
          let entry := "\"" + escape_str(pair.0) + "\": " + stringify(pair.1);
          step(i + 1, acc + (", " if i > 0 | "" if _) + entry)
        ) if _
      );
      "{" + step(0, "") + "}"
    )
  )
);
