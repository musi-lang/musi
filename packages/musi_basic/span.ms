import { new, push, get, length } from "@std/collections/array";
import { char_at, substring, trim_line_endings } from "@std/text/text";

export const Span := record {
  file: Nat,
  start: Nat,
  end: Nat
};

export const make := proc (file: Nat, start: Nat, end: Nat) -> Span {
  Span{ .file := file, .start := start, .end := end }
};

export const dummy := Span{ .file := 0, .start := 0, .end := 0 };

export const file := proc (span: Span) -> Nat {
  span.file
};

export const start := proc (span: Span) -> Nat {
  span.start
};

export const end := proc (span: Span) -> Nat {
  span.end
};

export const len := proc (span: Span) -> Nat {
  span.end - span.start
};

export const merge := proc (a: Span, b: Span) -> Span {
  const new_start := if b.start < a.start then b.start else a.start;
  const new_end := if b.end > a.end then b.end else a.end;
  Span{ .file := a.file, .start := new_start, .end := new_end }
};
