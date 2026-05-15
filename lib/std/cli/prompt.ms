--! Interactive command-line prompt helpers.

let io := import "@std/io";
let maybe := import "@std/maybe";
let text := import "@std/text";

let Maybe := maybe.Maybe;

--- Prompt and read one line.
export let prompt (message : String) :=
  (
    io.write(message);
    io.readLine()
  );

--- Read and trim one line.
export let readTrimmedLine () := io.readLine().trim();

--- Prompt, read, and trim one line.
export let promptTrimmed (message : String) := prompt(message).trim();

--- Read a non-empty trimmed line.
export let readNonEmptyLine () :=
  (
    let value := readTrimmedLine();
    if value.isEmpty() then maybe.None[String]() else maybe.Some[String](value);
  );
--- Prompt for a non-empty trimmed line.
export let promptNonEmpty (message : String) :=
  (
    let value := promptTrimmed(message);
    if value.isEmpty() then maybe.None[String]() else maybe.Some[String](value);
  );
