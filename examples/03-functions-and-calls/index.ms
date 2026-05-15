let io := import "@std/io";

export let add (left : Int, right : Int) := left + right;

export let square (value : Int) := value * value;

export let score (base : Int, bonus : Int) := square(add(base, bonus));

export let demo () := (add(20, 22), square(9), score(5, 2));

demo();
io.writeLn("functions: add, square, and nested score evaluated");
