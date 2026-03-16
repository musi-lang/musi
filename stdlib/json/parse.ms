//! JSON parser.

import "@std/rt" as rt;
import "./types" as types;

let CH_TAB := 9;
let CH_LF := 10;
let CH_CR := 13;
let CH_SPACE := 32;
let CH_QUOTE := 34;
let CH_PLUS := 43;
let CH_COMMA := 44;
let CH_MINUS := 45;
let CH_DOT := 46;
let CH_0 := 48;
let CH_9 := 57;
let CH_COLON := 58;
let CH_LBRACKET := 91;
let CH_BACKSLASH := 92;
let CH_RBRACKET := 93;
let CH_a := 97;
let CH_e := 101;
let CH_E := 69;
let CH_f := 102;
let CH_l := 108;
let CH_n := 110;
let CH_r := 114;
let CH_s := 115;
let CH_t := 116;
let CH_u := 117;
let CH_LBRACE := 123;
let CH_RBRACE := 125;

let ParseResult := choice {
    Ok of (types.JsonValue, Int)
  + Err of String
};

let is_ws := (ch) => (
  let c := rt.rune_to_int(ch);
  c = CH_SPACE or c = CH_TAB or c = CH_LF or c = CH_CR
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
  c >= CH_0 and c <= CH_9
);

let CH_SLASH := 47;
let RUNE_QUOTE := rt.rune_from_int(CH_QUOTE);
let RUNE_BACKSLASH := rt.rune_from_int(CH_BACKSLASH);
let RUNE_SLASH := rt.rune_from_int(CH_SLASH);
let RUNE_LF := rt.rune_from_int(CH_LF);
let RUNE_CR := rt.rune_from_int(CH_CR);
let RUNE_TAB := rt.rune_from_int(CH_TAB);

let parse_string := (chars, start) => (
  let len := rt.arr_len(chars);
  let mut buf := [];
  let step := (i) => (
    .Err("unterminated string") if i >= len
    | (
      let ch := chars.[i];
      let c  := rt.rune_to_int(ch);
      .Ok((rt.str_from_chars(buf), i + 1)) if c = CH_QUOTE
      | (
        (
          let nc := rt.rune_to_int(chars.[i + 1]);
          (rt.arr_push(buf, RUNE_QUOTE);    step(i + 2)) if nc = CH_QUOTE
          | (rt.arr_push(buf, RUNE_BACKSLASH); step(i + 2)) if nc = CH_BACKSLASH
          | (rt.arr_push(buf, RUNE_SLASH);  step(i + 2)) if nc = CH_SLASH
          | (rt.arr_push(buf, RUNE_LF);    step(i + 2)) if nc = CH_n
          | (rt.arr_push(buf, RUNE_CR);    step(i + 2)) if nc = CH_r
          | (rt.arr_push(buf, RUNE_TAB);   step(i + 2)) if nc = CH_t
          | .Err("unknown escape sequence") if _
        ) if i + 1 < len
        | .Err("unexpected end after backslash") if _
      ) if c = CH_BACKSLASH
      | (rt.arr_push(buf, ch); step(i + 1)) if _
    ) if _
  );
  step(start + 1)
);

let parse_number := (chars, start) => (
  let len := rt.arr_len(chars);
  let step := (i) => (
    (
      let c := rt.rune_to_int(chars.[i]);
      step(i + 1) if (c >= CH_0 and c <= CH_9)
        or c = CH_MINUS
        or c = CH_DOT
        or c = CH_e
        or c = CH_E
        or c = CH_PLUS
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
      .Ok((.Array(items), p1 + 1)) if ch = CH_RBRACKET
      | (
        match parse_value_ref(chars, p1) (
          .Ok((val, p2)) => (
            rt.arr_push(items, val);
            let p3 := skip_ws(chars, p2);
            (
              (
                let sep := rt.rune_to_int(chars.[p3]);
                step(p3 + 1) if sep = CH_COMMA
                | (
                  let p4 := skip_ws(chars, p3);
                  .Ok((.Array(items), p4 + 1)) if rt.rune_to_int(chars.[p4]) = CH_RBRACKET
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
      .Ok((.Object(pairs), p1 + 1)) if ch = CH_RBRACE
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
                          step(p6 + 1) if sep = CH_COMMA
                          | (
                            let p7 := skip_ws(chars, p6);
                            .Ok((.Object(pairs), p7 + 1)) if rt.rune_to_int(chars.[p7]) = CH_RBRACE
                            | .Err("expected ',' or '}' in object") if _
                          ) if _
                        ) if p6 < len
                        | .Err("unterminated object") if _
                      )
                    )
                    | .Err(e) => .Err(e)
                  )
                ) if rt.rune_to_int(chars.[p3]) = CH_COLON
                | .Err("expected ':' after object key") if _
              ) if p3 < len
              | .Err("unterminated object after key") if _
            )
            | .Err(e) => .Err(e)
          )
        ) if ch = CH_QUOTE
        | .Err("expected '\"' for object key") if _
      ) if _
    ) if p1 < len
    | .Err("unterminated object") if _
  );
  step(start + 1)
);

let char_eq := (chars, pos, expected) => (
  pos < rt.arr_len(chars) and rt.rune_to_int(chars.[pos]) = expected
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
    ) if c = CH_QUOTE
    | parse_array(chars, p)  if c = CH_LBRACKET
    | parse_object(chars, p) if c = CH_LBRACE
    | parse_number(chars, p) if c = CH_MINUS or is_digit_char(ch)
    | (
      (
        .Ok((.Bool(.True), p + 4))
          if char_eq(chars, p, CH_t)
            and char_eq(chars, p + 1, CH_r)
            and char_eq(chars, p + 2, CH_u)
            and char_eq(chars, p + 3, CH_e)
        | .Ok((.Bool(.False), p + 5))
          if char_eq(chars, p, CH_f)
            and char_eq(chars, p + 1, CH_a)
            and char_eq(chars, p + 2, CH_l)
            and char_eq(chars, p + 3, CH_s)
            and char_eq(chars, p + 4, CH_e)
        | .Ok((.Null, p + 4))
          if char_eq(chars, p, CH_n)
            and char_eq(chars, p + 1, CH_u)
            and char_eq(chars, p + 2, CH_l)
            and char_eq(chars, p + 3, CH_l)
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
