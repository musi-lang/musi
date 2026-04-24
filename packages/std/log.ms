let io := import "@std/io";
let text := import "@std/text";

--- Log severity level.
export opaque let Level := data {
  | Debug := 10
  | Info := 20
  | Warn := 30
  | Error := 40
};

@link(symbol := "data.tag")
native "musi" let levelTagIntrinsic (level : Level) : Int;

--- Debug log level.
export let debugLevel : Level := .Debug;

--- Informational log level.
export let infoLevel : Level := .Info;

--- Warning log level.
export let warnLevel : Level := .Warn;

--- Error log level.
export let errorLevel : Level := .Error;

--- Numeric level value.
export let (self : Level).value () : Int := unsafe {
  levelTagIntrinsic(self);
};

--- Lower-case level name.
export let (self : Level).name () : String :=
  match self (
  | .Debug => "debug"
  | .Info => "info"
  | .Warn => "warn"
  | .Error => "error"
  );

--- Write a log message.
export let write (level : Level, message : String) : Unit :=
  io.writeErrLine(
    text.concat(text.concat("[", level.name()), text.concat("] ", message))
  );

--- Write a debug message.
export let debug (message : String) : Unit := write(debugLevel, message);

--- Write an informational message.
export let info (message : String) : Unit := write(infoLevel, message);

--- Write a warning message.
export let warn (message : String) : Unit := write(warnLevel, message);

--- Write an error message.
export let error (message : String) : Unit := write(errorLevel, message);
