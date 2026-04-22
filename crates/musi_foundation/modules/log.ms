let Core := import "musi:core";
let Int := Core.Int;
let String := Core.String;
let Unit := Core.Unit;

export opaque let Log := effect {
  let info (message : String) : Unit;
  let write (level : Int, message : String) : Unit;
};

export let info (message : String) : Unit := ask Log.info(message);
export let write (level : Int, message : String) : Unit := ask Log.write(level, message);
