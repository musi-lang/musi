let Option := import "@std/option";
let Runtime := import "musi:runtime";

export let argsCount () : Int := Runtime.processArgCount();

export let argAt (index : Int) :=
  match Runtime.processArgAt(index) (
  | value if value = "" => Option.none[String]()
  | value => Option.some[String](value)
  );

export let cwd () : String := Runtime.processCwd();

export let run (command : String) : Int := Runtime.processRun(command);

export let exit (code : Int) : Unit := Runtime.processExit(code);
