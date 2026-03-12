import "./math";

#[entrypoint]
let main : () ~> under { IO } :=
    () => (
        let (s, c, a) := (square(5), cube(3), abs(0 - 42));
        writeln(f"square(5) = {s}");
        writeln(f"cube(3) = {c}");
        writeln(f"abs(0 - 42) = {a}")
    );

