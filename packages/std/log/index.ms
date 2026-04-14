let Runtime := import "musi:runtime";

export let debugLevel : Int := 10;
export let infoLevel : Int := 20;
export let warnLevel : Int := 30;
export let errorLevel : Int := 40;

export let levelName (level : Int) : String :=
  match () (
  | _ if level = debugLevel => "debug"
  | _ if level = infoLevel => "info"
  | _ if level = warnLevel => "warn"
  | _ => "error"
  );

export let write (level : Int, message : String) : Unit := Runtime.logWrite(level, message);

export let debug (message : String) : Unit := write(debugLevel, message);
export let info (message : String) : Unit := write(infoLevel, message);
export let warn (message : String) : Unit := write(warnLevel, message);
export let error (message : String) : Unit := write(errorLevel, message);
