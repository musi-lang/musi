let Runtime := import "musi:runtime";

export let join (left : String, right : String) : String := Runtime.pathJoin(left, right);

export let normalize (value : String) : String := Runtime.pathNormalize(value);

export let dirname (value : String) : String := Runtime.pathDirname(value);

export let basename (value : String) : String := Runtime.pathBasename(value);

export let extname (value : String) : String := Runtime.pathExtname(value);

export let isAbsolute (value : String) : Bool := Runtime.pathIsAbsolute(value) > 0;
