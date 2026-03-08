// std/collections/iter.ms — iterator combinators and generators
// import { range, range_step, enumerate, zip_arrays, ... } from "std/collections/iter"

/// Generate an array of integers from `start` (inclusive) to `end` (exclusive).
export fn range(start: Int, end: Int): []Int => (
    var result := [];
    var i := start;
    while i < end loop (
        array_push(result, i);
        i <- i + 1
    );
    result
);

/// Generate an array of integers from `start` to `end` with step `step`.
export fn range_step(start: Int, end: Int, step: Int): []Int => (
    var result := [];
    var i := start;
    if step > 0 then (
        while i < end loop (
            array_push(result, i);
            i <- i + step
        )
    ) else if step < 0 then (
        while i > end loop (
            array_push(result, i);
            i <- i + step
        )
    );
    result
);

/// A pair of index and value.
export record Indexed['T] { index: Int, value: 'T };

/// Enumerate an array, returning pairs of (index, value).
export fn enumerate['T](arr: []'T): []Indexed['T] => (
    var result := [];
    var i := 0;
    while i < array_length(arr) loop (
        array_push(result, Indexed.{ index := i, value := array_get(arr, i) });
        i <- i + 1
    );
    result
);

/// A pair of two values (for zip operations).
export record Pair['A, 'B] { first: 'A, second: 'B };

/// Zip two arrays into an array of pairs (stops at shorter length).
export fn zip_arrays['A, 'B](a: []'A, b: []'B): []Pair['A, 'B] => (
    var result := [];
    const len := if array_length(a) < array_length(b) then array_length(a) else array_length(b);
    var i := 0;
    while i < len loop (
        array_push(result, Pair.{ first := array_get(a, i), second := array_get(b, i) });
        i <- i + 1
    );
    result
);

/// Repeat a value `n` times.
export fn repeat_val['T](val: 'T, n: Int): []'T => (
    var result := [];
    var i := 0;
    while i < n loop (
        array_push(result, val);
        i <- i + 1
    );
    result
);

/// Cycle through an array `n` times (concatenates `n` copies).
export fn array_cycle['T](arr: []'T, n: Int): []'T => (
    var result := [];
    var i := 0;
    while i < n loop (
        for x in arr loop (array_push(result, x));
        i <- i + 1
    );
    result
);

/// Take elements while predicate is true.
export fn take_while['T](arr: []'T, pred: 'T -> Bool): []'T => (
    var result := [];
    var i := 0;
    var done := .False;
    while i < array_length(arr) and not done loop (
        const x := array_get(arr, i);
        if pred(x) then array_push(result, x)
        else done <- .True;
        i <- i + 1
    );
    result
);

/// Drop elements while predicate is true.
export fn drop_while['T](arr: []'T, pred: 'T -> Bool): []'T => (
    var result := [];
    var i := 0;
    var dropping := .True;
    while i < array_length(arr) loop (
        const x := array_get(arr, i);
        if dropping then (
            if pred(x) then ()
            else (
                dropping <- .False;
                array_push(result, x)
            )
        ) else array_push(result, x);
        i <- i + 1
    );
    result
);

/// Split an array into chunks of size `n`.
export fn chunks['T](arr: []'T, n: Int): [][]'T => (
    var result := [];
    var i := 0;
    const len := array_length(arr);
    while i < len loop (
        const end := if i + n < len then i + n else len;
        array_push(result, array_slice(arr, i, end));
        i <- i + n
    );
    result
);

/// Split an array into two at index `n`.
export fn split_at['T](arr: []'T, n: Int): Pair[[]'T, []'T] => (
    const len := array_length(arr);
    const mid := if n < 0 then 0 else if n > len then len else n;
    Pair.{
        first := array_slice(arr, 0, mid),
        second := array_slice(arr, mid, len)
    }
);

/// Partition an array into two based on a predicate.
export fn partition['T](arr: []'T, pred: 'T -> Bool): Pair[[]'T, []'T] => (
    var pass := [];
    var fail := [];
    for x in arr loop (
        if pred(x) then array_push(pass, x)
        else array_push(fail, x)
    );
    Pair.{ first := pass, second := fail }
);

/// Interleave two arrays (a[0], b[0], a[1], b[1], ...).
export fn interleave['T](a: []'T, b: []'T): []'T => (
    var result := [];
    const len := if array_length(a) > array_length(b) then array_length(a) else array_length(b);
    var i := 0;
    while i < len loop (
        if i < array_length(a) then array_push(result, array_get(a, i));
        if i < array_length(b) then array_push(result, array_get(b, i));
        i <- i + 1
    );
    result
);

/// Get the last element, or `.None` if empty.
export fn last['T](arr: []'T): Option['T] => (
    const len := array_length(arr);
    if len = 0 then .None
    else .Some(array_get(arr, len - 1))
);

/// Get the first element, or `.None` if empty.
export fn first['T](arr: []'T): Option['T] => (
    if array_length(arr) = 0 then .None
    else .Some(array_get(arr, 0))
);

/// Return all elements except the last.
export fn init['T](arr: []'T): []'T => (
    const len := array_length(arr);
    if len = 0 then []
    else array_slice(arr, 0, len - 1)
);

/// Return all elements except the first.
export fn tail['T](arr: []'T): []'T => (
    const len := array_length(arr);
    if len = 0 then []
    else array_slice(arr, 1, len)
);

/// Find the index of the first element satisfying a predicate.
export fn find_index['T](arr: []'T, pred: 'T -> Bool): Option[Int] => (
    var i := 0;
    var found := .None;
    while i < array_length(arr) loop (
        match found with (
            .Some(_) => ()
          | .None => if pred(array_get(arr, i)) then found <- .Some(i)
        );
        i <- i + 1
    );
    found
);
