let io := import "@std/io";

export let prepare (value : Int) :=
  (
    let doubled := value * 2;
    let shifted := doubled + 1;
    shifted
  );

export let demo () := (prepare(3), prepare(10));

demo();
io.writeLn("blocks: local lets ran and final expression returned");
