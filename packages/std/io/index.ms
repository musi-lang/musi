let runtime := import "musi:runtime";

--- Read one line from stdin.
export let readLine () : String := runtime.ioReadLine();

--- Write text to stdout.
export let write (value : String) : Unit := runtime.ioPrint(value);

--- Write text plus newline to stdout.
export let writeLine (value : String) : Unit := runtime.ioPrintLine(value);

--- Write text to stderr.
export let writeErr (value : String) : Unit := runtime.ioPrintError(value);

--- Write text plus newline to stderr.
export let writeErrLine (value : String) : Unit := runtime.ioPrintErrorLine(value);
