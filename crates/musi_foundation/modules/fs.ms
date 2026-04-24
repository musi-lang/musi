let Core := import "musi:core";
let Int := Core.Int;
let String := Core.String;
let Unit := Core.Unit;

export opaque let Fs := effect {
  let readText (path : String) : String;
  let writeText (path : String, text : String) : Unit;
  let exists (path : String) : Int;
  let appendText (path : String, text : String) : Int;
  let remove (path : String) : Int;
  let createDirAll (path : String) : Int;
};

export let readText (path : String) : String := ask Fs.readText(path);

export let writeText (path : String, text : String) : Unit :=
  ask Fs.writeText(path, text);

export let exists (path : String) : Int := ask Fs.exists(path);

export let appendText (path : String, text : String) : Int :=
  ask Fs.appendText(path, text);

export let remove (path : String) : Int := ask Fs.remove(path);

export let createDirAll (path : String) : Int := ask Fs.createDirAll(path);
