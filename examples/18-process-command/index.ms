let io := import "@std/io";
let process := import "@std/process";

export let demo () :=
  (
    io.writeLn("running a shell command from Musi");
    let code := process.run(
      process.command("printf 'hello from a child process\n'")
    );
    io.writeLn("child process finished");
    code
  );

demo();
io.writeLn("process example finished");
