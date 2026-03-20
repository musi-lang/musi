//! INI file parser.

import "@std/rt" as rt;

/// A parsed INI section with its name and key-value entries.
export let Section := record { name : String; entries : [](String, String) };

let is_comment_line := (line) => (
  let trimmed := rt.str_trim(line);
  rt.str_len(trimmed) = 0
  or rt.str_starts_with(trimmed, ";")
  or rt.str_starts_with(trimmed, "#")
);

let parse_section_name := (line) => (
  let trimmed := rt.str_trim(line);
  let len := rt.str_len(trimmed);
  (
    let inner := rt.str_slice(trimmed, 1, len - 1);
    rt.str_trim(inner)
  ) if rt.str_starts_with(trimmed, "[") and rt.str_ends_with(trimmed, "]")
  | "" if _
);

let _split_entry := (line) => (
  let trimmed := rt.str_trim(line);
  let eq_idx := rt.str_index_of(trimmed, "=");
  let key := rt.str_trim(rt.str_slice(trimmed, 0, eq_idx));
  let val := rt.str_trim(rt.str_slice(trimmed, eq_idx + 1, rt.str_len(trimmed)));
  (key, val)
);

/// Parses an INI-formatted string into a list of Section records.
export let parse := (input) => (
  let raw_lines := rt.str_split(input, "\n");
  let n := rt.arr_len(raw_lines);
  let mut sections := [];
  let mut current_name := "";
  let mut current_entries := [];
  let flush_section := () => (
    (
      rt.arr_push(sections, .{ name := current_name, entries := current_entries });
      current_entries <- []
    ) if rt.str_len(current_name) > 0 or rt.arr_len(current_entries) > 0
    | () if _
  );
  let step := (i) => (
    () if i >= n
    | (
      let line := rt.str_trim(raw_lines.[i]);
      (
        step(i + 1) if is_comment_line(line)
        | (
          let sec_name := parse_section_name(line);
          flush_section();
          current_name <- sec_name;
          current_entries <- [];
          step(i + 1)
        ) if rt.str_starts_with(line, "[")
        | (
          let eq_idx := rt.str_index_of(line, "=");
          (
            let key := rt.str_trim(rt.str_slice(line, 0, eq_idx));
            let val := rt.str_trim(rt.str_slice(line, eq_idx + 1, rt.str_len(line)));
            rt.arr_push(current_entries, (key, val));
            step(i + 1)
          ) if eq_idx >= 0
          | step(i + 1) if _
        ) if _
      )
    ) if _
  );
  step(0);
  flush_section();
  sections
);

/// Looks up a key in a named section, returning .Some(value) or .None.
export let get := (sections, section_name : String, key : String) => (
  let sn := rt.arr_len(sections);
  let find_section := (si) => (
    .None if si >= sn
    | (
      let sec := sections.[si];
      (
        let entries := sec.entries;
        let en := rt.arr_len(entries);
        let find_key := (ei) => (
          .None if ei >= en
          | (
            let pair : (String, String) := entries.[ei];
            (.Some(pair.1) if pair.0 = key | find_key(ei + 1) if _)
          ) if _
        );
        find_key(0)
      ) if sec.name = section_name
      | find_section(si + 1) if _
    ) if _
  );
  find_section(0)
);
