//! CSV parser (RFC 4180 subset).

import "@std/rt" as rt;

let parse_quoted_field := (s, start) => (
  let len := rt.str_len(s);
  let mut acc_chars := [];
  let step := (i) => (
    (acc_chars, i) if i >= len
    | (
      let c := rt.str_at(s, i);
      let c_int := rt.rune_to_int(c);
      (
        (
          let next_int := (rt.rune_to_int(rt.str_at(s, i + 1)) if i + 1 < len | -1 if _);
          (
            (rt.arr_push(acc_chars, c); step(i + 2)) if next_int = 34
            | (acc_chars, i + 1) if _
          )
        ) if c_int = 34
        | (rt.arr_push(acc_chars, c); step(i + 1)) if _
      )
    ) if _
  );
  step(start + 1)
);

export let parse_row := (line) => (
  let len := rt.str_len(line);
  let mut fields := [];
  let step := (i) => (
    () if i > len
    | (
      let c_int := (rt.rune_to_int(rt.str_at(line, i)) if i < len | -1 if _);
      (
        let (field_chars, end_pos) := parse_quoted_field(line, i);
        rt.arr_push(fields, rt.str_from_chars(field_chars));
        step(end_pos + 1)
      ) if c_int = 34
      | (
        let next_comma := rt.str_index_of(rt.str_slice(line, i, len), ",");
        (
          (
            rt.arr_push(fields, rt.str_slice(line, i, i + next_comma));
            step(i + next_comma + 1)
          ) if next_comma >= 0
          | (
            rt.arr_push(fields, rt.str_slice(line, i, len));
            step(len + 1)
          ) if _
        )
      ) if _
    ) if _
  );
  step(0);
  fields
);

export let parse := (input) => (
  let raw_lines := rt.str_split(input, "\n");
  let n := rt.arr_len(raw_lines);
  let mut rows := [];
  let step := (i) => (
    () if i >= n
    | (
      let line := rt.str_trim(raw_lines.[i]);
      (
        (rt.arr_push(rows, parse_row(line)); step(i + 1)) if rt.str_len(line) > 0
        | step(i + 1) if _
      )
    ) if _
  );
  step(0);
  rows
);
