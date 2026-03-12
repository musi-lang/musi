let greet : (String) ~> () under { IO } :=
    (name) => writeln(f"hello, {name}!");

#[entrypoint]
let main : () ~> () under { IO } :=
    () => (
        greet("world");
        greet("musi")
    );
