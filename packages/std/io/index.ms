let option := import "@std/option";
let runtime := import "musi:runtime";
let text := import "@std/text";

export let readLine () : String := runtime.ioReadLine();

export let write (value : String) : Unit := runtime.ioPrint(value);

export let writeLine (value : String) : Unit := runtime.ioPrintLine(value);

export let writeErr (value : String) : Unit := runtime.ioPrintError(value);

export let writeErrLine (value : String) : Unit := runtime.ioPrintErrorLine(value);

export let prompt (message : String) : String :=
  (
    write(message);
    readLine()
  );

export let readTrimmedLine () : String := text.trim(readLine());

export let promptTrimmed (message : String) : String := text.trim(prompt(message));

export let readNonEmptyLine () :=
  match readTrimmedLine() (
  | value if text.isEmpty(value) => option.none[String]()
  | value => option.some[String](value)
  );

export let promptNonEmpty (message : String) :=
  match promptTrimmed(message) (
  | value if text.isEmpty(value) => option.none[String]()
  | value => option.some[String](value)
  );
