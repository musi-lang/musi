import { create, push, get, set, length } from "stdlib/collections/array";
import { char_at, substring, trim_line_endings } from "stdlib/text/text";
import { is_newline, is_carriage_return } from "stdlib/text/char";

export const SourceFile := record {
  path: Text,
  source: Text,
  lines: [Nat]
};

export const Source := record {
  files: [SourceFile]
};

export const empty := Source{ .files := create(0) };

export const add_file := proc (source: Source, path: Text, source_text: Text) -> (Source, Nat) {
  const lines := compute_lines(source_text);
  const file := SourceFile{ .path := path, .source := source_text, .lines := lines };
  push(source.files, file);
  (source, length(source.files) - 1)
};

export const get_file := proc (source: Source, id: Nat) -> SourceFile {
  get(source.files, id)
};

export const line_col := proc (file: SourceFile, offset: Nat) -> (Nat, Nat) {
  // Simple binary search for line number
  const lo := 0;
  const hi := length(file.lines) - 1;
  const line := 0;

  while lo <= hi do {
    const mid := (lo + hi) / 2;
    const mid_line := get(file.lines, mid);
    if mid_line <= offset then {
      line <- mid;
      lo <- mid + 1
    } else {
      hi <- mid - 1
    }
  };

  const line_num := line + 1;
  const col_num := offset - get(file.lines, line) + 1;

  (line_num, col_num)
};

export const line_text := proc (file: SourceFile, line_num: Nat) -> Text {
  if line_num < 1 or else line_num > length(file.lines) then {
    ""
  } else {
    const start := get(file.lines, line_num - 1);
    const end_ := if line_num < length(file.lines) then {
      get(file.lines, line_num)
    } else {
      // Go to end of source
      0  // Placeholder - would need source length
    };

    if end_ <= start then {
      ""
    } else {
      const line_len := end_ - start;
      const raw_line := substring(file.source, start, line_len);
      trim_line_endings(raw_line)
    }
  }
};

export const path := proc (file: SourceFile) -> Text {
  file.path
};

export const compute_lines := proc (source: Text) -> [Nat] {
  const lines := create(100);
  push(lines, 0);  // First line starts at 0

  const i := 0;
  const source_len := 0;  // Placeholder - would need actual length

  // For now, just return basic line array
  // Real implementation would scan through source text
  while i < source_len do {
    const c := char_at(source, i);
    if is_newline(c) then {
      push(lines, i + 1);
      i <- i + 1
    } else if is_carriage_return(c) then {
      // Check for CRLF
      if i + 1 < source_len then {
        const next_c := char_at(source, i + 1);
        if is_newline(next_c) then {
          push(lines, i + 2);
          i <- i + 2
        } else {
          push(lines, i + 1);
          i <- i + 1
        }
      } else {
        push(lines, i + 1);
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  };

  lines
};
