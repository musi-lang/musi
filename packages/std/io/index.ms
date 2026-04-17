let option := import "@std/option";
let Option := option.Option;
let runtime := import "musi:runtime";
let text := import "@std/text";

/// Read one line from stdin.
export let readLine () : String := runtime.ioReadLine();

/// Write text to stdout.
export let write (value : String) : Unit := runtime.ioPrint(value);

/// Write text plus newline to stdout.
export let writeLine (value : String) : Unit := runtime.ioPrintLine(value);

/// Write text to stderr.
export let writeErr (value : String) : Unit := runtime.ioPrintError(value);

/// Write text plus newline to stderr.
export let writeErrLine (value : String) : Unit := runtime.ioPrintErrorLine(value);

/// Prompt and read one line.
export let prompt (message : String) : String :=
  (
    write(message);
    readLine()
  );

/// Read and trim one line.
export let readTrimmedLine () : String := text.trim(readLine());

/// Prompt, read, and trim one line.
export let promptTrimmed (message : String) : String := text.trim(prompt(message));

/// Read a non-empty trimmed line.
export let readNonEmptyLine () : Option[String] :=
  match readTrimmedLine() (
  | value if text.isEmpty(value) => option.none[String]()
  | value => option.some[String](value)
  );

/// Prompt for a non-empty trimmed line.
export let promptNonEmpty (message : String) : Option[String] :=
  match promptTrimmed(message) (
  | value if text.isEmpty(value) => option.none[String]()
  | value => option.some[String](value)
  );
