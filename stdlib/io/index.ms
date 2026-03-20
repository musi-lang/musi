//! Console I/O via FFI to libc getchar/putchar.

import "@std/rt" as rt;

foreign "C" (
  let getchar : () ~> Int;
  let putchar : (Int) ~> Int;
);

/// Writes a single rune to stdout.
export let put_char := (r) => putchar(rt.rune_to_int(r));

/// Writes a newline character to stdout.
export let newline := () => putchar(10);

/// Reads one rune from stdin, returning .None on EOF.
export let read_char := () => (
  let c := getchar();
  .Some(rt.rune_from_int(c)) if c >= 0
  | .None if _
);

/// Reads a line from stdin, stopping at newline or EOF.
export let read_line := () => (
  let mut chars := [];
  let step := () => (
    let c := getchar();
    (
      rt.str_from_chars(chars) if c < 0
      | rt.str_from_chars(chars) if c = 10
      | (rt.arr_push(chars, rt.rune_from_int(c)); step()) if _
    )
  );
  step()
);

/// Reads all of stdin until EOF.
export let read_all := () => (
  let mut chars := [];
  let step := () => (
    let c := getchar();
    (
      rt.str_from_chars(chars) if c < 0
      | (rt.arr_push(chars, rt.rune_from_int(c)); step()) if _
    )
  );
  step()
);

/// Writes msg to stdout then reads and returns a line from stdin.
export let prompt := (msg) => (
  rt.write(msg);
  read_line()
);
