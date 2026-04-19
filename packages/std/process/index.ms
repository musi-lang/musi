let option := import "@std/option";
let Option := option.Option;
let path := import "@std/path";
let Path := path.Path;
let runtime := import "musi:runtime";

--- Process command text.
export opaque let Command := data {
  | Command(value : String)
};

--- Process exit code.
export opaque let ExitCode := data {
  | ExitCode(value : Int)
};

--- Build a shell command.
export let command (value : String) : Command := .Command(value := value);

--- Build an exit code.
export let exitCode (value : Int) : ExitCode := .ExitCode(value := value);

--- Command as raw string.
export let(self : Command).toString () : String :=
  match self (
  | .Command(value := value) => value
  );

--- Exit code as raw int.
export let(self : ExitCode).toInt () : Int :=
  match self (
  | .ExitCode(value := value) => value
  );

--- Number of process args.
export let argsCount () : Int := runtime.processArgCount();

--- Process arg by index.
export let argAt (index : Int) : Option[String] :=
  match runtime.processArgAt(index) (
  | value if value = "" => option.noneOf[String]()
  | value => option.someOf[String](value)
  );

--- Current working directory.
export let cwd () : Path := path.fromString(runtime.processCwd());

--- Run a shell command.
export let run (target : Command) : ExitCode :=
  exitCode(runtime.processRun(target.toString()));

--- Exit current process.
export let exit (code : ExitCode) : Unit := runtime.processExit(code.toInt());
