val abs := fn(x: Int) -> Int {
  if x < 0 then -x else x
};

val min := fn(x: Int, y: Int) -> Int {
  if x < y then x else y
};

val max := fn(x: Int, y: Int) -> Int {
  if x > y then x else y
};

val clamp := fn(value: Int, min_val: Int, max_val: Int) -> Int {
  if value < min_val then min_val
  else if value > max_val then max_val
  else value
};

export { abs, min, max, clamp };
