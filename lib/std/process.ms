--! Process module.

let maybe := import "@std/maybe";
let Maybe := maybe.Maybe;
let path := import "@std/path";
let Path := path.Path;
let processHost := import "musi:process";

--- Process command text.
export hidden let Command := data {
  | CommandText(value : String)
};

--- Process exit code.
export hidden let ExitCode := data {
  | ExitStatus(value : Int)
};

--- Build a shell command.
export let command (value : String) := .CommandText(value := value);

--- Build an exit code.
export let exitCode (value : Int) := .ExitStatus(value := value);

--- Command as raw string.
export let (self : Command).toString () :=
  match self (
  | .CommandText(value := value) => value
  );

--- Exit code as raw int.
export let (self : ExitCode).toInt () :=
  match self (
  | .ExitStatus(value := value) => value
  );

--- Number of process args.
export let argsCount () := processHost.argCount();

--- Process arg by index.
export let argAt (index : Int) :=
  (
    let value := processHost.argAt(index);
    if value = "" then maybe.None[String]() else maybe.Some[String](value)
  );

--- Current working directory.
export let cwd () := path.fromString(processHost.cwd());

--- Run a shell command.
export let run (target : Command) :=
  exitCode(processHost.run(target.toString()));

--- Exit current process.
export let exit (code : ExitCode) := processHost.exit(code.toInt());
