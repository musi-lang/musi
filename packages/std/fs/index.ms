let path := import "@std/path";
let Path := path.Path;
let runtime := import "musi:runtime";

--- Returns true when a path exists.
export let exists (target : Path) : Bool := runtime.fsExists(target.toString()) > 0;

--- Read a UTF-8 text file.
export let readText (target : Path) : String := runtime.fsReadText(target.toString());

--- Write a UTF-8 text file.
export let writeText (target : Path, value : String) : Unit := runtime.fsWriteText(target.toString(),
  value
);

--- Append UTF-8 text to a file.
export let appendText (target : Path, value : String) : Bool := runtime.fsAppendText(target.toString(),
  value
) > 0;

--- Remove a file or directory entry.
export let remove (target : Path) : Bool := runtime.fsRemove(target.toString()) > 0;

--- Create a directory and missing parents.
export let createDirAll (target : Path) : Bool := runtime.fsCreateDirAll(target.toString()) > 0;
