let runtime := import "musi:runtime";

export opaque let Level := data {
  | Debug := 10
  | Info := 20
  | Warn := 30
  | Error := 40
};

@link(symbol := "data.tag")
foreign "musi" let levelTagIntrinsic (level : Level) : Int;

export let debugLevel () : Level := .Debug;
export let infoLevel () : Level := .Info;
export let warnLevel () : Level := .Warn;
export let errorLevel () : Level := .Error;

export let levelValue (level : Level) : Int :=
  unsafe { levelTagIntrinsic(level); };

export let levelName (level : Level) : String :=
  match level (
  | .Debug => "debug"
  | .Info => "info"
  | .Warn => "warn"
  | .Error => "error"
  );

export let write (level : Level, message : String) : Unit := runtime.logWrite(levelValue(level), message);

export let debug (message : String) : Unit := write(.Debug, message);
export let info (message : String) : Unit := write(.Info, message);
export let warn (message : String) : Unit := write(.Warn, message);
export let error (message : String) : Unit := write(.Error, message);
