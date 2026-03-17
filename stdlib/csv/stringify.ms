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
  let mut cells := [];
  let step := (i) => (
    () if i >= n
    | (
      let raw := fields.[i];
      rt.arr_push(cells, (quote_field(raw) if needs_quoting(raw) | raw if _));
      step(i + 1)
    ) if _
  );
  step(0);
  rt.str_join(cells, ",")
);

export let stringify := (rows) => (
  let n := rt.arr_len(rows);
  let mut lines := [];
  let step := (i) => (
    () if i >= n
    | (rt.arr_push(lines, serialize_row(rows.[i])); step(i + 1)) if _
  );
  step(0);
  rt.str_join(lines, "\n")
);
