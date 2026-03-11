/// Join a list of showable values with a separator.
///
/// Each element is converted to a string via `show` before joining.
///
/// # Examples
///
/// ```musi
/// join([1, 2, 3], ", ")  -- "1, 2, 3"
/// ```
export let join ['T] where 'T <: Show : (List of 'T, String) -> String :=
    (xs, sep) => (
        let go(ys) := match ys (
            .Cons(x, .Nil) => show(x)
          | .Cons(x, rest) => show(x) + sep + go(rest)
          | .Nil => ""
        );
        go(xs)
    );

/// Pad a string on the left to the given width with a fill rune.
///
/// # Examples
///
/// ```musi
/// pad_left("42", 5, ' ')  -- "   42"
/// ```
export let pad_left : (String, Int, Rune) -> String;

/// Pad a string on the right to the given width with a fill rune.
export let pad_right : (String, Int, Rune) -> String;

/// Repeat a string `n` times.
///
/// # Examples
///
/// ```musi
/// repeat("ab", 3)  -- "ababab"
/// ```
export let repeat : (String, Int) -> String :=
    (s, n) => (
        let go(acc, i) := (acc if i <= 0 | go(acc + s, i - 1) if _);
        go("", n)
    );
