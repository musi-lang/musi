foreign "libm" let sqrt : (Float64) -> Float64;

#[entrypoint]
let main : () ~> () under { IO } :=
    () => (
        let x := 144.0;
        let result := sqrt(x);
        writeln(f"sqrt({x}) = {result}")
    );
