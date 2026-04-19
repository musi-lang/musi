let io := import "@std/io";
let option := import "@std/option";
let text := import "@std/text";

let Option := option.Option;

--- Prompt and read one line.
export let prompt (message : String) : String :=
  (
    io.write(message);
    io.readLine()
  );

--- Read and trim one line.
export let readTrimmedLine () : String := io.readLine().trim();

--- Prompt, read, and trim one line.
export let promptTrimmed (message : String) : String := prompt(message).trim();

--- Read a non-empty trimmed line.
export let readNonEmptyLine () : Option[String] :=
  match readTrimmedLine() (
  | value if value.isEmpty() => option.noneOf[String]()
  | value => option.someOf[String](value)
  );

--- Prompt for a non-empty trimmed line.
export let promptNonEmpty (message : String) : Option[String] :=
  match promptTrimmed(message) (
  | value if value.isEmpty() => option.noneOf[String]()
  | value => option.someOf[String](value)
  );
