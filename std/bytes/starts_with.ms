let check (h : []Nat8, n : []Nat8, i : Int) : Bool :=
    (
      | .True if i = n.len
      | .False if h.[i] /= n.[i]
      | check(h, n, i + 1) if _
    );

/// Check whether haystack starts with the bytes in needle.
export let starts_with (haystack : []Nat8, needle : []Nat8) : Bool :=
    (
      | .False if haystack.len < needle.len
      | (.True if needle.len = 0 | check(haystack, needle, 0) if _) if _
    );
