// std/core/Array.ms
// Pure-Musi array utilities. Import with: import { ... } from "std/core/Array"

export fn array_map['T, 'U](arr: []'T, f: 'T -> 'U): []'U => (
  var result := [];
  for x in arr loop (
    array_push(result, f(x))
  );
  result
);

export fn array_filter['T](arr: []'T, pred: 'T -> Bool): []'T => (
  var result := [];
  for x in arr loop (
    if pred(x) then array_push(result, x)
  );
  result
);

export fn array_fold['T, 'U](arr: []'T, init: 'U, f: ('U, 'T) -> 'U): 'U => (
  var acc := init;
  for x in arr loop (
    acc <- f(acc, x)
  );
  acc
);

export fn array_each['T](arr: []'T, f: 'T -> Unit): Unit => (
  for x in arr loop (
    f(x)
  )
);

export fn array_contains['T](arr: []'T, val: 'T): Bool => (
  var found := False;
  for x in arr loop (
    if x = val then found <- True
  );
  found
);

export fn array_reverse['T](arr: []'T): []'T => (
  var result := [];
  var i := array_length(arr) - 1;
  while i >= 0 loop (
    array_push(result, array_get(arr, i));
    i <- i - 1
  );
  result
);

export fn array_concat['T](a: []'T, b: []'T): []'T => (
  var result := [];
  for x in a loop (array_push(result, x));
  for x in b loop (array_push(result, x));
  result
);
