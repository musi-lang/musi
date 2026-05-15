let io := import "@std/io";

export let clampHealth (health : Int) :=
  if health < 0 then 0 else if health > 100 then 100 else health;

export let status (health : Int) :=
  if health <= 0 then "down" else if health < 30 then "critical" else "ready";

export let demo () :=
  (clampHealth(-10), clampHealth(55), clampHealth(140), status(20));

demo();
io.writeLn("if expressions: health was clamped and classified");
