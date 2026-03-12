export let square : (Int) -> Int :=
    (x) => x * x;

export let cube : (Int) -> Int :=
    (x) => x * x * x;

export let abs : (Int) -> Int :=
    (x) => (
        x if x >= 0
      | 0 - x if _
    );
