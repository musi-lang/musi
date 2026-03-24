/// Check whether haystack starts with the bytes in needle.
export let starts_with (haystack : []U8, needle : []U8) : Bool :=
    (
        .False if haystack.len < needle.len
      | (.True if needle.len = 0 | check(haystack, needle, 0) if _) if _
    );

let check (h : []U8, n : []U8, i : Int) : Bool :=
    (
        .True if i = n.len
      | .False if h.[i] /= n.[i]
      | check(h, n, i + 1) if _
    );
