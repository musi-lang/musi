--! Formatting module.

let datetime := import "@std/datetime";
let text := import "@std/text";
let fmtHost := import "musi:fmt";
let Duration := datetime.Duration;

--- Format integer value.
export let int (value : Int) := fmtHost.int(value);

--- Format floating-point value.
export let float (value : Float) := fmtHost.float(value);

--- Format boolean value.
export let bool (value : Bit) := if value then "true" else "false";

--- Format duration as milliseconds.
export let duration (value : Duration) :=
  text.concat(fmtHost.int(datetime.durationMillis(value)), "ms");

--- Format byte count as decimal bytes.
export let bytesCount (value : Int) := text.concat(fmtHost.int(value), " B");
