let path := import "@std/path";
let Path := path.Path;
let fsHost := import "musi:fs";

--- Returns true when a path exists.
export let exists (target : Path) : Bool :=
  fsHost.exists(target.toString()) > 0;

--- Read a UTF-8 text file.
export let readText (target : Path) : String :=
  fsHost.readText(target.toString());

--- Write a UTF-8 text file.
export let writeText (target : Path, value : String) : Unit :=
  fsHost.writeText(target.toString(), value);

--- Append UTF-8 text to a file.
export let appendText (target : Path, value : String) : Bool :=
  fsHost.appendText(target.toString(), value) > 0;

--- Remove a file or directory entry.
export let remove (target : Path) : Bool :=
  fsHost.remove(target.toString()) > 0;

--- Create a directory and missing parents.
export let createDirAll (target : Path) : Bool :=
  fsHost.createDirAll(target.toString()) > 0;
