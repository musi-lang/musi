let text := import "@std/text";

--- Platform path value.
export opaque let Path := data {
  | PathText(value : String)
};

--- Build a path from raw text.
export let fromString (value : String) : Path := .PathText(value := value);

--- Core operation for `toString`.
export let toString (target : Path) : String :=
  match target (
  | .PathText(value := value) => value
  );

let slash : String := "/";

let rec lastByteIndexFrom (
  source : String,
  needle : Int,
  index : Int,
  best : Int
) : Int :=
  match () (
  | _ if index >= source.length() => best
  | _ if text.byteAt(source, index) = needle => lastByteIndexFrom(
    source,
    needle,
    index + 1,
    index,
  )
  | _ => lastByteIndexFrom(source, needle, index + 1, best)
  );

let lastByteIndex (source : String, needle : Int) : Int :=
  lastByteIndexFrom(source, needle, 0, - 1);

--- Core operation for `join`.
export let join (target : Path, other : Path) : Path :=
  (
    let left := toString(target);
    let right := toString(other);
    match () (
    | _ if left = "" => fromString(right)
    | _ if right = "" => fromString(left)
    | _ if text.byteAt(right, 0) = 47 => fromString(right)
    | _ if text.byteAt(left, left.length() - 1) = 47 => fromString(
      text.concat(left, right)
    )
    | _ => fromString(text.concat(text.concat(left, slash), right))
    )
  );

--- Core operation for `normalize`.
export let normalize (target : Path) : Path := target;

--- Core operation for `dirName`.
export let dirName (target : Path) : Path :=
  (
    let source := toString(target);
    let index := lastByteIndex(source, 47);
    match () (
    | _ if index < 0 => fromString("")
    | _ if index = 0 => fromString(slash)
    | _ => fromString(text.slice(source, 0, index))
    )
  );

--- Core operation for `baseName`.
export let baseName (target : Path) : String :=
  (
    let source := toString(target);
    let index := lastByteIndex(source, 47);
    match () (
    | _ if index < 0 => source
    | _ => text.slice(source, index + 1, source.length())
    )
  );

--- Core operation for `extName`.
export let extName (target : Path) : String :=
  (
    let base := baseName(target);
    let index := lastByteIndex(base, 46);
    match () (
    | _ if index <= 0 => ""
    | _ => text.slice(base, index, base.length())
    )
  );

--- Core operation for `isAbsolute`.
export let isAbsolute (target : Path) : Bool :=
  text.byteAt(toString(target), 0) = 47;

--- Return raw path text.
export let (self : Path).toString () : String := toString(self);

--- Join two paths.
export let (self : Path).join (other : Path) : Path := join(self, other);

--- Normalize a path.
export let (self : Path).normalize () : Path := normalize(self);

--- Parent directory name.
export let (self : Path).dirName () : Path := dirName(self);

--- Base file name.
export let (self : Path).baseName () : String := baseName(self);

--- Extension name, including leading `.` when present.
export let (self : Path).extName () : String := extName(self);

--- Returns true for absolute paths.
export let (self : Path).isAbsolute () : Bool := isAbsolute(self);
