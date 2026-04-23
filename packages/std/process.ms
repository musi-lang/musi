let option := import "@std/option";
let Option := option.Option;
let path := import "@std/path";
let Path := path.Path;
let processHost := import "musi:process";

--- Process command text.
export opaque let Command := data {
  | CommandText(value : String)
};

--- Process exit code.
export opaque let ExitCode := data {
  | ExitStatus(value : Int)
};

--- Build a shell command.
export let command (value : String) : Command := .CommandText(value := value);

--- Build an exit code.
export let exitCode (value : Int) : ExitCode := .ExitStatus(value := value);

--- Command as raw string.
export let (self : Command).toString () : String :=
  match self (
  | .CommandText(value := value) => value
  );

--- Exit code as raw int.
export let (self : ExitCode).toInt () : Int :=
  match self (
  | .ExitStatus(value := value) => value
  );

--- Number of process args.
export let argsCount () : Int := processHost.argCount();

--- Process arg by index.
export let argAt (index : Int) : Option[String] :=
  match processHost.argAt(index) (
  | value if value = "" => option.noneOf[String]()
  | value => option.someOf[String](value)
  );

--- Current working directory.
export let cwd () : Path := path.fromString(processHost.cwd());

--- Run a shell command.
export let run (target : Command) : ExitCode :=
  exitCode(processHost.run(target.toString()));

--- Exit current process.
export let exit (code : ExitCode) : Unit := processHost.exit(code.toInt());
