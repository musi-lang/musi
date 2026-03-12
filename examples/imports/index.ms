import "./math";

#[entrypoint]
let main : () ~> () under { IO } :=
    () => (
        let s := square(5);
        writeln(f"square(5) = {s}");
        let c := cube(3);
        writeln(f"cube(3) = {c}");
        let a := abs(0 - 42);
        writeln(f"abs(-42) = {a}")
    );
