let ioHost := import "musi:io";

--- Read one line from stdin.
export let readLine () : String := ioHost.readLine();

--- Write text to stdout.
export let write (value : String) : Unit := ioHost.print(value);

--- Write text plus newline to stdout.
export let writeLine (value : String) : Unit := ioHost.printLine(value);

--- Write text to stderr.
export let writeErr (value : String) : Unit := ioHost.printError(value);

--- Write text plus newline to stderr.
export let writeErrLine (value : String) : Unit := ioHost.printErrorLine(value);
