//! CSV serializer.

import "@std/rt" as rt;

let needs_quoting := (field) => (
  rt.str_contains(field, ",")
  or rt.str_contains(field, "\"")
  or rt.str_contains(field, "\n")
  or rt.str_contains(field, "\r")
);

let quote_field := (field) => (
  let escaped := rt.str_replace(field, "\"", "\"\"");
  f"\"{escaped}\""
);

let serialize_row := (fields) => (
  let n := rt.arr_len(fields);
  "" if n = 0
  | (
    let step := (acc, i) => (
      acc if i >= n
      | (
        let raw := fields.[i];
        let cell := (quote_field(raw) if needs_quoting(raw) | raw if _);
        step(acc + "," + cell, i + 1)
      ) if _
    );
    let first := fields.[0];
    let first_cell := (quote_field(first) if needs_quoting(first) | first if _);
    step(first_cell, 1)
  ) if _
);

export let stringify := (rows) => (
  let n := rt.arr_len(rows);
  "" if n = 0
  | (
    let step := (acc, i) => (
      acc if i >= n
      | step(acc + "\n" + serialize_row(rows.[i]), i + 1) if _
    );
    step(serialize_row(rows.[0]), 1)
  ) if _
);
