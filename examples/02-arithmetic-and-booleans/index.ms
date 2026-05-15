let io := import "@std/io";

export let subtotal := 40 + 2;

export let withDiscount := subtotal - 5;

export let doubled := withDiscount * 2;

export let shared := doubled / 3;

export let inBudget := shared < 30;

export let demo () := (subtotal, withDiscount, doubled, shared, inBudget);

demo();
io.writeLn("arithmetic: subtotal, discount, division, comparison evaluated");
