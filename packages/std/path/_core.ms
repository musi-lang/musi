let runtime := import "musi:runtime";

--- Platform path value.
export opaque let Path := data {
  | Path(value : String)
};

--- Build a path from raw text.
export let fromString (value : String) : Path := .Path(value := value);

--- Internal implementation for `toString`.
export let toString (target : Path) : String :=
  match target (
  | .Path(value := value) => value
  );

--- Internal implementation for `join`.
export let join (target : Path, other : Path) : Path :=
  fromString(runtime.pathJoin(toString(target), toString(other)));

--- Internal implementation for `normalize`.
export let normalize (target : Path) : Path :=
  fromString(runtime.pathNormalize(toString(target)));

--- Internal implementation for `dirName`.
export let dirName (target : Path) : Path :=
  fromString(runtime.pathDirname(toString(target)));

--- Internal implementation for `baseName`.
export let baseName (target : Path) : String :=
  runtime.pathBasename(toString(target));

--- Internal implementation for `extName`.
export let extName (target : Path) : String :=
  runtime.pathExtname(toString(target));

--- Internal implementation for `isAbsolute`.
export let isAbsolute (target : Path) : Bool :=
  runtime.pathIsAbsolute(toString(target)) > 0;
