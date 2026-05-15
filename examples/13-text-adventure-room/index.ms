let io := import "@std/io";

export hidden let Room := data {
  | Room(name : String, north : String, item : String)
};

export let start := .Room(name := "hall", north := "gallery", item := "key");

export let gallery := .Room(name := "gallery", north := "tower", item := "map");

export let describe (room : Room) :=
  match room (
  | .Room(name := name, north := north, item := item) => (
    name,
    "north",
    north,
    item,
  )
  );

export let demo () := (describe(start), describe(gallery));

demo();
io.writeLn("text adventure: room descriptions generated");
