let core := import "./_core.ms";

--- Platform path value.
export let Path := core.Path;

--- Build a path from raw text.
export let fromString := core.fromString;

--- Return raw path text.
export let(self : Path).toString () : String := core.toString(self);

--- Join two paths.
export let(self : Path).join (other : Path) : Path := core.join(self, other);

--- Normalize a path.
export let(self : Path).normalize () : Path := core.normalize(self);

--- Parent directory name.
export let(self : Path).dirName () : Path := core.dirName(self);

--- Base file name.
export let(self : Path).baseName () : String := core.baseName(self);

--- Extension name, including leading `.` when present.
export let(self : Path).extName () : String := core.extName(self);

--- Returns true for absolute paths.
export let(self : Path).isAbsolute () : Bool := core.isAbsolute(self);
