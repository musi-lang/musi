let Core := import "musi:core";
let String := Core.String;
let Unit := Core.Unit;

export opaque let Io := effect {
  let print (text : String) : Unit;
  let printLine (text : String) : Unit;
  let printError (text : String) : Unit;
  let printErrorLine (text : String) : Unit;
  let readLine () : String;
};

export let print (text : String) : Unit := ask Io.print(text);

export let printLine (text : String) : Unit := ask Io.printLine(text);

export let printError (text : String) : Unit := ask Io.printError(text);

export let printErrorLine (text : String) : Unit := ask Io.printErrorLine(text);

export let readLine () : String := ask Io.readLine();
