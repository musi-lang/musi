#[entrypoint]
let main : () ~> under { IO } :=
    () => writeln("hello, world!");
