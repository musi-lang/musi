/// Join a list of strings with a separator.
export let join : forall 'T where 'T <: Show -> (List of 'T, String) -> String :=
    forall 'T where 'T <: Show -> (xs, sep) -> (
        let go(ys) := match ys (
            .Cons(x, .Nil) => show(x)
          | .Cons(x, rest) => show(x) + sep + go(rest)
          | .Nil => ""
        );
        go(xs)
    );

/// Pad a string on the left to the given width.
export let pad_left : (String, Int, Rune) -> String;

/// Pad a string on the right to the given width.
export let pad_right : (String, Int, Rune) -> String;

/// Repeat a string `n` times.
export let repeat : (String, Int) -> String :=
    (s, n) -> (
        let go(acc, i) := (acc if i <= 0 | go(acc + s, i - 1) if _);
        go("", n)
    );
