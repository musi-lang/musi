let io := import "@std/io";

export hidden let Todo := data {
  | Todo(title : String, done : Bit)
};

export let markDone (todo : Todo) :=
  match todo (
  | .Todo(title := title, done := done) => .Todo(title := title, done := 1 = 1)
  );

export let label (todo : Todo) :=
  match todo (
  | .Todo(title := title, done := done) => (title, done)
  );

export let demo () :=
  (
    label(.Todo(title := "learn values", done := 1 = 0)),
    label(markDone(.Todo(title := "run examples", done := 1 = 0))),
  );

demo();
io.writeLn("todo model: item state changed to done");
