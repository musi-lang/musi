//! File path manipulation (string-based, no I/O).

import "@std/rt" as rt;

export let join := (a, b) => (
  "" if rt.str_len(b) = 0
  | b if rt.str_len(a) = 0
  | b if rt.str_starts_with(b, "/")
  | (
    (rt.str_slice(a, 0, rt.str_len(a) - 1) + "/" + b) if rt.str_ends_with(a, "/")
    | a + "/" + b if _
  ) if _
);

export let dirname := (p) => (
  let trimmed := (
    rt.str_slice(p, 0, rt.str_len(p) - 1) if rt.str_ends_with(p, "/") and rt.str_len(p) > 1
    | p if _
  );
  let slash_idx := (
    let step := (i, last) => (
      last if i < 0
      | (
        let c_int := rt.rune_to_int(rt.str_at(trimmed, i));
        (i if c_int = 47 | step(i - 1, last) if _) if _
      ) if _
    );
    step(rt.str_len(trimmed) - 1, -1)
  );
  (
    "/" if slash_idx = 0
    | "." if slash_idx = -1
    | rt.str_slice(trimmed, 0, slash_idx) if _
  )
);

export let basename := (p) => (
  let trimmed := (
    rt.str_slice(p, 0, rt.str_len(p) - 1) if rt.str_ends_with(p, "/") and rt.str_len(p) > 1
    | p if _
  );
  let len := rt.str_len(trimmed);
  let slash_idx := (
    let step := (i, last) => (
      last if i < 0
      | (
        let c_int := rt.rune_to_int(rt.str_at(trimmed, i));
        (i if c_int = 47 | step(i - 1, last) if _) if _
      ) if _
    );
    step(len - 1, -1)
  );
  (
    trimmed if slash_idx = -1
    | rt.str_slice(trimmed, slash_idx + 1, len) if _
  )
);

export let extension := (p) => (
  let base := basename(p);
  let len := rt.str_len(base);
  let dot_idx := (
    let step := (i, last) => (
      last if i < 0
      | (
        let c_int := rt.rune_to_int(rt.str_at(base, i));
        (i if c_int = 46 | step(i - 1, last) if _) if _
      ) if _
    );
    step(len - 1, -1)
  );
  (
    .None if dot_idx <= 0
    | .Some(rt.str_slice(base, dot_idx + 1, len)) if _
  )
);

export let with_extension := (p, ext) => (
  let base := basename(p);
  let dir := dirname(p);
  let len := rt.str_len(base);
  let dot_idx := (
    let step := (i, last) => (
      last if i < 0
      | (
        let c_int := rt.rune_to_int(rt.str_at(base, i));
        (i if c_int = 46 | step(i - 1, last) if _) if _
      ) if _
    );
    step(len - 1, -1)
  );
  let stem := (
    base if dot_idx <= 0
    | rt.str_slice(base, 0, dot_idx) if _
  );
  let new_base := (
    stem if rt.str_len(ext) = 0
    | stem + "." + ext if _
  );
  (
    new_base if dir = "."
    | join(dir, new_base) if _
  )
);

export let is_absolute := (p) => rt.str_starts_with(p, "/");

export let normalize := (p) => (
  let is_abs := is_absolute(p);
  let parts := rt.str_split(p, "/");
  let n := rt.arr_len(parts);
  let mut stack := [];
  let step := (i) => (
    () if i >= n
    | (
      let part := parts.[i];
      (
        step(i + 1) if rt.str_len(part) = 0 or part = "."
        | (
          (
            let slen := rt.arr_len(stack);
            (
              (rt.arr_pop(stack); step(i + 1)) if slen > 0 and stack.[slen - 1] /= ".."
              | (rt.arr_push(stack, ".."); step(i + 1)) if _
            )
          )
        ) if part = ".."
        | (rt.arr_push(stack, part); step(i + 1)) if _
      )
    ) if _
  );
  step(0);
  let slen := rt.arr_len(stack);
  let joined := (rt.str_join(stack, "/") if slen > 0 | "" if _);
  (
    "/" + joined if is_abs
    | "." if rt.str_len(joined) = 0
    | joined if _
  )
);
