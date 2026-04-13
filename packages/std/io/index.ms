let Runtime := import "musi:runtime";

export let readLine () : String := Runtime.ioReadLine();

export let write (value : String) : Unit := Runtime.ioPrint(value);

export let writeLine (value : String) : Unit := Runtime.ioPrintLine(value);

export let writeError (value : String) : Unit := Runtime.ioPrintError(value);

export let writeErrorLine (value : String) : Unit := Runtime.ioPrintErrorLine(value);
