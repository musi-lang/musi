// std/collections/array.ms — array/slice utilities (arrays are []'T, backed by the VM)
// import { array_map, array_filter, array_fold, array_find, array_contains, ... } from "std/collections/array"

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

export fn array_flatten['T](arrs: [][]'T): []'T => (
    var result := [];
    for inner in arrs loop (
        for x in inner loop (array_push(result, x))
    );
    result
);

export fn array_take['T](arr: []'T, n: Int): []'T => (
    const len := array_length(arr);
    array_slice(arr, 0, if n < len then n else len)
);

export fn array_drop['T](arr: []'T, n: Int): []'T => (
    const len := array_length(arr);
    array_slice(arr, if n < len then n else len, len)
);

fn array_merge_sorted['T](a: []'T, b: []'T, cmp: ('T, 'T) -> Bool): []'T => (
    var result := [];
    var i := 0;
    var j := 0;
    while i < array_length(a) and j < array_length(b) loop (
        if cmp(array_get(a, i), array_get(b, j))
        then (array_push(result, array_get(a, i)); i <- i + 1)
        else (array_push(result, array_get(b, j)); j <- j + 1)
    );
    while i < array_length(a) loop (array_push(result, array_get(a, i)); i <- i + 1);
    while j < array_length(b) loop (array_push(result, array_get(b, j)); j <- j + 1);
    result
);

export fn array_sort_by['T](arr: []'T, cmp: ('T, 'T) -> Bool): []'T => (
    const n := array_length(arr);
    if n <= 1 then arr
    else (
        const mid := n / 2;
        array_merge_sorted(
            array_sort_by(array_slice(arr, 0, mid), cmp),
            array_sort_by(array_slice(arr, mid, n), cmp),
            cmp
        )
    )
);

export fn array_min_by['T](arr: []'T, cmp: ('T, 'T) -> Bool): Option['T] => (
    if array_length(arr) = 0 then .None
    else (
        var best := array_get(arr, 0);
        for x in arr loop (if cmp(x, best) then best <- x);
        .Some(best)
    )
);

export fn array_max_by['T](arr: []'T, cmp: ('T, 'T) -> Bool): Option['T] => (
    if array_length(arr) = 0 then .None
    else (
        var best := array_get(arr, 0);
        for x in arr loop (if cmp(best, x) then best <- x);
        .Some(best)
    )
);

export fn array_dedup['T](arr: []'T): []'T => (
    if array_length(arr) = 0 then []
    else (
        var result := [];
        array_push(result, array_get(arr, 0));
        var i := 1;
        while i < array_length(arr) loop (
            if array_get(arr, i) = array_get(arr, i - 1)
            then ()
            else array_push(result, array_get(arr, i));
            i <- i + 1
        );
        result
    )
);

export fn array_count_by['T](arr: []'T, pred: 'T -> Bool): Int => (
    var n := 0;
    for x in arr loop (if pred(x) then n <- n + 1);
    n
);
