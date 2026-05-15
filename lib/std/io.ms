--! Input and output module.

let ioHost := import "musi:io";

--- Read one line from stdin.
export let readLine () := ioHost.readLine();

--- Write text to stdout.
export let write (value : String) := ioHost.write(value);

--- Write text plus newline to stdout.
export let writeLn (value : String) := ioHost.writeLn(value);

--- Write text to stderr.
export let writeErr (value : String) := ioHost.writeErr(value);

--- Write text plus newline to stderr.
export let writeErrLn (value : String) := ioHost.writeErrLn(value);
