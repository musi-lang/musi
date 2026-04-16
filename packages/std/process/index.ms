let option := import "@std/option";
let runtime := import "musi:runtime";

export let argsCount () : Int := runtime.processArgCount();

export let argAt (index : Int) :=
  match runtime.processArgAt(index) (
  | value if value = "" => option.none[String]()
  | value => option.some[String](value)
  );

export let cwd () : String := runtime.processCwd();

export let run (command : String) : Int := runtime.processRun(command);

export let exit (code : Int) : Unit := runtime.processExit(code);
