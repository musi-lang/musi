const __builtin_string_len := unsafe extern "C" proc (s: Text) -> Int;
const __builtin_string_get_char := unsafe extern "C" proc (s: Text, idx: Int) -> Int;
const __builtin_string_sub := unsafe extern "C" proc (s: Text, start: Int, len: Int) -> Text;
const __builtin_string_eq := unsafe extern "C" proc (a: Text, b: Text) -> Bool;

import { create } from "collections/array";

export const length := proc (s: Text) -> Int {
  __builtin_string_len(s)
};

export const char_at := proc (s: Text, idx: Int) -> Int {
  __builtin_string_get_char(s, idx)
};

export const substring := proc (s: Text, start: Int, len: Int) -> Text {
  __builtin_string_sub(s, start, len)
};

export const equals := proc (a: Text, b: Text) -> Bool {
  __builtin_string_eq(a, b)
};

export const starts_with := proc (s: Text, prefix: Text) -> Bool {
  const prefix_len := length(prefix);
  const s_len := length(s);

  if prefix_len > s_len then {
    false
  } else {
    const sub := substring(s, 0, prefix_len);
    equals(sub, prefix)
  }
};

export const ends_with := proc (s: Text, suffix: Text) -> Bool {
  const suffix_len := length(suffix);
  const s_len := length(s);

  if suffix_len > s_len then {
    false
  } else {
    const start := s_len - suffix_len;
    const sub := substring(s, start, suffix_len);
    equals(sub, suffix)
  }
};

export const contains := proc (s: Text, substring: Text) -> Bool {
  const sub_len := length(substring);
  const s_len := length(s);
  const i := 0;

  while i <= s_len - sub_len do {
    const candidate := __builtin_string_sub(s, i, sub_len);
    if equals(candidate, substring) then {
      return true
    };
    i <- i + 1
  };

  false
};

// TODO: Add panic builtin for error handling
// const panic := unsafe extern "C" proc (message: Text) -> Never;

export const split_lines := proc (s: Text) -> [Text] {
  const lines := create(0);
  const start := 0;
  const len := length(s);
  const i := 0;

  while i < len do {
    const c := char_at(s, i);
    if c = 10 then {  // newline
      const line_len := i - start;
      const line := substring(s, start, line_len);

      // Trim trailing carriage return if present
      const trimmed := if line_len > 0 then {
        const last_char := char_at(line, line_len - 1);
        if last_char = 13 then {  // carriage return
          substring(line, 0, line_len - 1)
        } else {
          line
        }
      } else {
        line
      };

      // This is a bit tricky with arrays - for now, we'll use a simple approach
      // In a real implementation, we'd want dynamic array operations
      // For now, let's just return an array with the whole string
      [s]
    } else {
      i <- i + 1
    }
  };

  [s]
};

export const trim_newline := proc (s: Text) -> Text {
  const len := length(s);
  if len > 0 then {
    const last_char := char_at(s, len - 1);
    if last_char = 10 then {  // newline
      substring(s, 0, len - 1)
    } else {
      s
    }
  } else {
    s
  }
};

export const trim_carriage_return := proc (s: Text) -> Text {
  const len := length(s);
  if len > 0 then {
    const last_char := char_at(s, len - 1);
    if last_char = 13 then {  // carriage return
      substring(s, 0, len - 1)
    } else {
      s
    }
  } else {
    s
  }
};

export const trim_line_endings := proc (s: Text) -> Text {
  trim_carriage_return(trim_newline(s))
};
