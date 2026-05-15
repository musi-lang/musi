--! Path manipulation helpers.

let text := import "@std/text";

--- Platform path value.
export hidden let Path := data {
  | PathText(value : String)
};

--- Build a path from raw text.
export let fromString (value : String) := .PathText(value := value);

--- Function for `toString`.
let pathToString (target : Path) :=
  match target (
  | .PathText(value := value) => value
  );

--- Return raw path text.
export let toString (target : Path) := pathToString(target);

let slash := "/";

let recur lastByteIndexFrom (
  source : String,
  needle : Int,
  index : Int,
  best : Int
) := if index >= source.length() then best
  else if text.byteAt(source, index) = needle then lastByteIndexFrom(
    source,
    needle,
    index + 1,
    index,
  ) else lastByteIndexFrom(source, needle, index + 1, best);

let lastByteIndex (source : String, needle : Int) :=
  lastByteIndexFrom(source, needle, 0, -1);

--- Function for `join`.
let joinPath (target : Path, other : Path) :=
  (
    let left := pathToString(target);
    let right := pathToString(other);
    if left = "" then fromString(right)
    else if right = "" then fromString(left)
    else if text.byteAt(right, 0) = 47 then fromString(right)
    else if text.byteAt(left, left.length() - 1) = 47 then fromString(
      text.concat(left, right)
    ) else fromString(text.concat(text.concat(left, slash), right))
  );

--- Function for `normalize`.
let normalizePath (target : Path) := target;

--- Function for `dirName`.
let pathDirName (target : Path) :=
  (
    let source := pathToString(target);
    let index := lastByteIndex(source, 47);
    if index < 0 then fromString("")
    else if index = 0 then fromString(slash)
    else fromString(text.slice(source, 0, index))
  );

--- Function for `baseName`.
let pathBaseName (target : Path) :=
  (
    let source := pathToString(target);
    let index := lastByteIndex(source, 47);
    if index < 0 then source
    else text.slice(source, index + 1, source.length())
  );

--- Function for `extName`.
let pathExtName (target : Path) :=
  (
    let base := pathBaseName(target);
    let index := lastByteIndex(base, 46);
    if index <= 0 then "" else text.slice(base, index, base.length())
  );

--- Function for `isAbsolute`.
let pathIsAbsolute (target : Path) := text.byteAt(pathToString(target), 0) = 47;

--- Join two paths, using other unchanged when it is absolute.
export let join (target : Path, other : Path) := joinPath(target, other);

--- Return normalized path text.
export let normalize (target : Path) := normalizePath(target);

--- Return parent directory path.
export let dirName (target : Path) := pathDirName(target);

--- Return final path component.
export let baseName (target : Path) := pathBaseName(target);

--- Return extension from final path component.
export let extName (target : Path) := pathExtName(target);

--- Returns true when path starts at root.
export let isAbsolute (target : Path) := pathIsAbsolute(target);

--- Return raw path text.
export let (self : Path).toString () := pathToString(self);

--- Join two paths.
export let (self : Path).join (other : Path) := joinPath(self, other);

--- Normalize a path.
export let (self : Path).normalize () := normalizePath(self);

--- Parent directory name.
export let (self : Path).dirName () := pathDirName(self);

--- Base file name.
export let (self : Path).baseName () := pathBaseName(self);

--- Extension name, including leading `.` when Some.
export let (self : Path).extName () := pathExtName(self);

--- Returns true for absolute paths.
export let (self : Path).isAbsolute () := pathIsAbsolute(self);
