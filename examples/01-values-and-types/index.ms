let io := import "@std/io";

export let answer := 42;

export let greeting := "typed values";

export let byte := 255n8;

export let pair := (answer, greeting);

export let demo () := (answer, greeting, byte, pair);

demo();
io.writeLn("values: Int, String, Nat8, and tuple are built");
