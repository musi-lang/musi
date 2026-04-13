let Runtime := import "musi:runtime";

export let exists (path : String) : Bool := Runtime.fsExists(path) > 0;

export let readText (path : String) : String := Runtime.fsReadText(path);

export let writeText (path : String, value : String) : Unit := Runtime.fsWriteText(path, value);

export let appendText (path : String, value : String) : Bool := Runtime.fsAppendText(path, value) > 0;

export let remove (path : String) : Bool := Runtime.fsRemove(path) > 0;

export let createDirAll (path : String) : Bool := Runtime.fsCreateDirAll(path) > 0;
