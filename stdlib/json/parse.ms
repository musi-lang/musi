//! JSON parser.

import "@std/rt" as rt;
import "./types" as types;

let ParseResult := choice {
    Ok of (types.JsonValue, Int)
  + Err of String
};

let is_ws := (ch) => (
  let c := rt.rune_to_int(ch);
  c = 32 or c = 9 or c = 10 or c = 13
);

let skip_ws := (chars, pos) => (
  let len := rt.arr_len(chars);
  let step := (i) => (
    step(i + 1) if i < len and is_ws(chars.[i])
    | i if _
  );
  step(pos)
);

let is_digit_char := (ch) => (
  let c := rt.rune_to_int(ch);
  c >= 48 and c <= 57
);

let parse_string := (chars, start) => (
  let len := rt.arr_len(chars);
  let step := (i, acc) => (
    .Err("unterminated string") if i >= len
    | (
      let ch := chars.[i];
      let c  := rt.rune_to_int(ch);
      .Ok((acc, i + 1)) if c = 34
      | (
        (
          let next := chars.[i + 1];
          let nc   := rt.rune_to_int(next);
          step(i + 2, acc + "\"") if nc = 34
          | step(i + 2, acc + "\\") if nc = 92
          | step(i + 2, acc + "/")  if nc = 47
          | step(i + 2, acc + "\n") if nc = 110
          | step(i + 2, acc + "\r") if nc = 114
          | step(i + 2, acc + "\t") if nc = 116
          | step(i + 2, acc)        if _
        ) if i + 1 < len
        | .Err("unexpected end after backslash") if _
      ) if c = 92
      | step(i + 1, acc + rt.str_from_chars([ch])) if _
    ) if _
  );
  step(start + 1, "")
);

let parse_number := (chars, start) => (
  let len := rt.arr_len(chars);
  let step := (i) => (
    (
      let c := rt.rune_to_int(chars.[i]);
      step(i + 1) if is_digit_char(chars.[i])
        or c = 45
        or c = 46
        or c = 101
        or c = 69
        or c = 43
      | i if _
    ) if i < len
    | i if _
  );
  let end_pos := step(start);
  let num_str := rt.str_from_chars(rt.arr_slice(chars, start, end_pos));
  .Ok((.Number(rt.str_parse_float(num_str)), end_pos))
);

let mut parse_value_ref := (_chars, _pos) => .Err("uninitialised");

let parse_array := (chars, start) => (
  let len := rt.arr_len(chars);
  let mut items := [];
  let step := (pos) => (
    let p1 := skip_ws(chars, pos);
    (
      let ch := rt.rune_to_int(chars.[p1]);
      .Ok((.Array(items), p1 + 1)) if ch = 93
      | (
        match parse_value_ref(chars, p1) (
          .Ok((val, p2)) => (
            rt.arr_push(items, val);
            let p3 := skip_ws(chars, p2);
            (
              (
                let sep := rt.rune_to_int(chars.[p3]);
                step(p3 + 1) if sep = 44
                | (
                  let p4 := skip_ws(chars, p3);
                  .Ok((.Array(items), p4 + 1)) if rt.rune_to_int(chars.[p4]) = 93
                  | .Err("expected ',' or ']' in array") if _
                ) if _
              ) if p3 < len
              | .Err("unterminated array") if _
            )
          )
          | .Err(e) => .Err(e)
        )
      ) if _
    ) if p1 < len
    | .Err("unterminated array") if _
  );
  step(start + 1)
);

let parse_object := (chars, start) => (
  let len := rt.arr_len(chars);
  let mut pairs := [];
  let step := (pos) => (
    let p1 := skip_ws(chars, pos);
    (
      let ch := rt.rune_to_int(chars.[p1]);
      .Ok((.Object(pairs), p1 + 1)) if ch = 125
      | (
        (
          match parse_string(chars, p1) (
            .Ok((key, p2)) => (
              let p3 := skip_ws(chars, p2);
              (
                (
                  let p4 := skip_ws(chars, p3 + 1);
                  match parse_value_ref(chars, p4) (
                    .Ok((val, p5)) => (
                      rt.arr_push(pairs, (key, val));
                      let p6 := skip_ws(chars, p5);
                      (
                        (
                          let sep := rt.rune_to_int(chars.[p6]);
                          step(p6 + 1) if sep = 44
                          | (
                            let p7 := skip_ws(chars, p6);
                            .Ok((.Object(pairs), p7 + 1)) if rt.rune_to_int(chars.[p7]) = 125
                            | .Err("expected ',' or '}' in object") if _
                          ) if _
                        ) if p6 < len
                        | .Err("unterminated object") if _
                      )
                    )
                    | .Err(e) => .Err(e)
                  )
                ) if rt.rune_to_int(chars.[p3]) = 58
                | .Err("expected ':' after object key") if _
              ) if p3 < len
              | .Err("unterminated object after key") if _
            )
            | .Err(e) => .Err(e)
          )
        ) if ch = 34
        | .Err("expected '\"' for object key") if _
      ) if _
    ) if p1 < len
    | .Err("unterminated object") if _
  );
  step(start + 1)
);

let parse_value := (chars, pos) => (
  let len := rt.arr_len(chars);
  let p := skip_ws(chars, pos);
  .Err("unexpected end of input") if p >= len
  | (
    let ch := chars.[p];
    let c  := rt.rune_to_int(ch);
    parse_string(chars, p) |> (r) => match r (
      .Ok((s, np)) => .Ok((.Str(s), np))
      | .Err(e)    => .Err(e)
    ) if c = 34
    | parse_array(chars, p)  if c = 91
    | parse_object(chars, p) if c = 123
    | parse_number(chars, p) if c = 45 or is_digit_char(ch)
    | (
      (
        .Ok((.Bool(.True), p + 4)) if rt.str_starts_with(rt.str_from_chars(rt.arr_slice(chars, p, len)), "true")
        | .Ok((.Bool(.False), p + 5)) if rt.str_starts_with(rt.str_from_chars(rt.arr_slice(chars, p, len)), "false")
        | .Ok((.Null, p + 4)) if rt.str_starts_with(rt.str_from_chars(rt.arr_slice(chars, p, len)), "null")
        | .Err("unexpected token") if _
      )
    ) if _
  ) if _
);

parse_value_ref <- parse_value;

export let parse := (s) => (
  let chars := rt.str_chars(s);
  match parse_value(chars, 0) (
    .Ok((v, _)) => .Ok(v)
    | .Err(e)   => .Err(e)
  )
);
