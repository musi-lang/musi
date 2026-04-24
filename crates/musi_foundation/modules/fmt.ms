let Core := import "musi:core";
let Float := Core.Float;
let Int := Core.Int;
let String := Core.String;

export opaque let Format := effect {
  let int (value : Int) : String;
  let float (value : Float) : String;
};

export let int (value : Int) : String := ask Format.int(value);

export let float (value : Float) : String := ask Format.float(value);
