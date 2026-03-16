//! Console I/O via FFI to libc getchar/putchar.

import "@std/rt" as rt;

foreign "C" (
  let getchar : () ~> Int;
  let putchar : (Int) ~> Int;
);

export let put_char := (r) => putchar(rt.rune_to_int(r));

export let newline := () => putchar(10);

export let read_char := () => (
  let c := getchar();
  .Some(rt.rune_from_int(c)) if c >= 0
  | .None if _
);

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

export let prompt := (msg) => (
  rt.write(msg);
  read_line()
);
