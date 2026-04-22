let Core := import "musi:core";
let Int := Core.Int;
let String := Core.String;
let Unit := Core.Unit;

export opaque let Process := effect {
  let argCount () : Int;
  let argAt (index : Int) : String;
  let cwd () : String;
  let run (command : String) : Int;
  let exit (code : Int) : Unit;
};

export let argCount () : Int := ask Process.argCount();
export let argAt (index : Int) : String := ask Process.argAt(index);
export let cwd () : String := ask Process.cwd();
export let run (command : String) : Int := ask Process.run(command);
export let exit (code : Int) : Unit := ask Process.exit(code);
