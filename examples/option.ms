choice Option { Some(Int) | None };

fn unwrap_or(opt: Option, default: Int): Int (
    match opt with
        case Some(v) => v
        case None => default
);

const a := Some(42);
const b := None;
writeln(int_to_string(unwrap_or(a, 0)));
writeln(int_to_string(unwrap_or(b, 0)));
