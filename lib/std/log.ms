--! Logging module.

let io := import "@std/io";
let text := import "@std/text";

--- Log severity level.
export hidden let Level := data {
  | Debug := 10
  | Info := 20
  | Warn := 30
  | Error := 40
};

--- Debug log level.
export let debugLevel := .Debug;

--- Informational log level.
export let infoLevel := .Info;

--- Warning log level.
export let warnLevel := .Warn;

--- Error log level.
export let errorLevel := .Error;

--- Numeric level value.
export let (self : Level).value () :=
  match self (
  | .Debug => 10
  | .Info => 20
  | .Warn => 30
  | .Error => 40
  );

--- Lower-case level name.
export let (self : Level).name () :=
  match self (
  | .Debug => "debug"
  | .Info => "info"
  | .Warn => "warn"
  | .Error => "error"
  );

--- Write a log message.
export let write (level : Level, message : String) :=
  io.writeErrLn(
    text.concat(text.concat("[", level.name()), text.concat("] ", message))
  );

--- Write a debug message.
export let debug (message : String) := write(debugLevel, message);

--- Write an informational message.
export let info (message : String) := write(infoLevel, message);

--- Write a warning message.
export let warn (message : String) := write(warnLevel, message);

--- Write an error message.
export let error (message : String) := write(errorLevel, message);
