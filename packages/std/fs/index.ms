let runtime := import "musi:runtime";

export let exists (path : String) : Bool := runtime.fsExists(path) > 0;

export let readText (path : String) : String := runtime.fsReadText(path);

export let writeText (path : String, value : String) : Unit := runtime.fsWriteText(path, value);

export let appendText (path : String, value : String) : Bool := runtime.fsAppendText(path, value) > 0;

export let remove (path : String) : Bool := runtime.fsRemove(path) > 0;

export let createDirAll (path : String) : Bool := runtime.fsCreateDirAll(path) > 0;
