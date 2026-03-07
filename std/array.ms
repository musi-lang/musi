// std/array.ms — array/slice utilities (arrays are []'T, backed by the VM)
// import { array_map, array_filter, array_fold, array_find, array_contains, ... } from "std/array"

export fn array_map['T, 'U](arr: []'T, f: 'T -> 'U): []'U => (
    var result := [];
    for x in arr loop (array_push(result, f(x)));
    result
);

export fn array_filter['T](arr: []'T, pred: 'T -> Bool): []'T => (
    var result := [];
    for x in arr loop (if pred(x) then array_push(result, x));
    result
);

export fn array_fold['T, 'U](arr: []'T, init: 'U, f: ('U, 'T) -> 'U): 'U => (
    var acc := init;
    for x in arr loop (acc <- f(acc, x));
    acc
);

export fn array_each['T](arr: []'T, f: 'T -> Unit): Unit => (
    for x in arr loop (f(x))
);

export fn array_any['T](arr: []'T, pred: 'T -> Bool): Bool => (
    var found := .False;
    for x in arr loop (if pred(x) then found <- .True);
    found
);

export fn array_all['T](arr: []'T, pred: 'T -> Bool): Bool => (
    var ok := .True;
    for x in arr loop (if pred(x) then () else ok <- .False);
    ok
);

export fn array_find['T](arr: []'T, pred: 'T -> Bool): Option['T] => (
    var result := .None;
    for x in arr loop (
        if pred(x) then (
            if is_none(result) then result <- .Some(x)
        )
    );
    result
);

fn is_none['T](opt: Option['T]): Bool => match opt with ( .None => .True | _ => .False );

export fn array_contains['T](arr: []'T, val: 'T): Bool => (
    var found := .False;
    for x in arr loop (if x = val then found <- .True);
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

export fn array_flat_map['T, 'U](arr: []'T, f: 'T -> []'U): []'U => (
    var result := [];
    for x in arr loop (
        const inner := f(x);
        for y in inner loop (array_push(result, y))
    );
    result
);

export fn array_zip['A, 'B](a: []'A, b: []'B, f: ('A, 'B) -> Unit): Unit => (
    var i := 0;
    const len := if array_length(a) < array_length(b) then array_length(a) else array_length(b);
    while i < len loop (
        f(array_get(a, i), array_get(b, i));
        i <- i + 1
    )
);

export fn array_sum(arr: []Int): Int => (
    var total := 0;
    for x in arr loop (total <- total + x);
    total
);
