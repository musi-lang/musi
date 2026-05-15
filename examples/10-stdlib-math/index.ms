let io := import "@std/io";
let math := import "@std/math";

export let viewportWidth := 640;

export let viewportHeight := 480;

export let clampX (x : Int) := math.clamp(x, 0, viewportWidth - 1);

export let clampY (y : Int) := math.clamp(y, 0, viewportHeight - 1);

export let distanceFromOrigin (x : Int, y : Int) := math.abs(x) + math.abs(y);

export let demo () :=
  (clampX(-4), clampX(900), clampY(500), distanceFromOrigin(-3, 9));

demo();
io.writeLn("stdlib math: clamp and abs helpers ran");
