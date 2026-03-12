import "./math" as math;

#[entrypoint]
let main : () ~> under { IO } :=
    () => (
        let (s, c, a) := (math.square(5), math.cube(3), math.abs(0 - 42));
        writeln(f"square(5) = {s}");
        writeln(f"cube(3) = {c}");
        writeln(f"abs(0 - 42) = {a}")
    );

