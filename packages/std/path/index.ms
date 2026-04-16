let runtime := import "musi:runtime";

export let join (left : String, right : String) : String := runtime.pathJoin(left, right);

export let normalize (value : String) : String := runtime.pathNormalize(value);

export let dirName (value : String) : String := runtime.pathDirname(value);

export let baseName (value : String) : String := runtime.pathBasename(value);

export let extName (value : String) : String := runtime.pathExtname(value);

export let isAbsolute (value : String) : Bool := runtime.pathIsAbsolute(value) > 0;
