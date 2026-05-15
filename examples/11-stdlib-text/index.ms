let io := import "@std/io";
let text := import "@std/text";

export let title := "  Musi Playground  ";

export let trimmed := text.trim(title);

export let shout := text.toUpperCase(trimmed);

export let label := text.concat(shout, "!");

export let demo () := (trimmed, shout, label, text.length(label));

demo();
io.writeLn("stdlib text: trim, uppercase, concat, and length ran");
