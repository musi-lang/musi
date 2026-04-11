export let abs (value : Int) : Int :=
    case () of (
        | _ if value >= 0 => value
        | _ => -value
    );

export let abs_float (value : Float) : Float :=
    case () of (
        | _ if value >= 0.0 => value
        | _ => -value
    );

export let max (left : Int, right : Int) : Int :=
    case () of (
        | _ if left >= right => left
        | _ => right
    );

export let min (left : Int, right : Int) : Int :=
    case () of (
        | _ if left <= right => left
        | _ => right
    );

export let clamp (value : Int, low : Int, high : Int) : Int :=
    case () of (
        | _ if value < low => low
        | _ if value > high => high
        | _ => value
    );
