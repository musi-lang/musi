let Option := import "@std/option";
let Runtime := import "musi:runtime";
let Text := import "@std/text";

export let readLine () : String := Runtime.ioReadLine();

export let write (value : String) : Unit := Runtime.ioPrint(value);

export let writeLine (value : String) : Unit := Runtime.ioPrintLine(value);

export let writeError (value : String) : Unit := Runtime.ioPrintError(value);

export let writeErrorLine (value : String) : Unit := Runtime.ioPrintErrorLine(value);

export let prompt (message : String) : String :=
  (
    write(message);
    readLine()
  );

export let readTrimmedLine () : String := Text.trim(readLine());

export let promptTrimmed (message : String) : String := Text.trim(prompt(message));

export let readNonEmptyLine () :=
  match readTrimmedLine() (
  | value if Text.isEmpty(value) => Option.none[String]()
  | value => Option.some[String](value)
  );

export let promptNonEmpty (message : String) :=
  match promptTrimmed(message) (
  | value if Text.isEmpty(value) => Option.none[String]()
  | value => Option.some[String](value)
  );
