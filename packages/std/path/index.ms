let runtime := import "musi:runtime";

/// Platform path value.
export opaque let Path := data {
  | Path(value : String)
};

/// Build a path from raw text.
export let fromString (value : String) : Path := .Path(value := value);

/// Return raw path text.
export let (self : Path).toString () : String :=
  match self (
  | .Path(value := value) => value
  );

/// Join two paths.
export let (self : Path).join (other : Path) : Path :=
  fromString(runtime.pathJoin(toString(self), toString(other)));

/// Normalize a path.
export let (self : Path).normalize () : Path :=
  fromString(runtime.pathNormalize(toString(self)));

/// Parent directory name.
export let (self : Path).dirName () : Path :=
  fromString(runtime.pathDirname(toString(self)));

/// Base file name.
export let (self : Path).baseName () : String := runtime.pathBasename(toString(self));

/// Extension name, including leading dot when present.
export let (self : Path).extName () : String := runtime.pathExtname(toString(self));

/// Returns true for absolute paths.
export let (self : Path).isAbsolute () : Bool := runtime.pathIsAbsolute(toString(self)) > 0;
