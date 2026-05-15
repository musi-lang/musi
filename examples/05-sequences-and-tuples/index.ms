let io := import "@std/io";

export let numbers := [1, 2, 3, 4];

export let rows := [[1, 2], [3, 4]];

export let player := ("Ada", 7, 7 > 0);

export let echo (value) := value;

export let demo () := (echo(numbers), echo(rows), echo(player));

demo();
io.writeLn("sequences: lists, nested rows, and tuple built");
