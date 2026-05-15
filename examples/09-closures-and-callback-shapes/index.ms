let io := import "@std/io";

export let makeAdder (delta : Int) := \(value : Int) => value + delta;

export let add3 := makeAdder(3);

export let twiceAdd3 (value : Int) := add3(add3(value));

export let demo () := (add3(7), twiceAdd3(10));

demo();
io.writeLn("closures: captured adder ran twice");
