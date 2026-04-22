let Core := import "musi:core";
let Int := Core.Int;
let String := Core.String;

export opaque let PathHost := effect {
  let join (left : String, right : String) : String;
  let normalize (value : String) : String;
  let dirname (value : String) : String;
  let basename (value : String) : String;
  let extname (value : String) : String;
  let isAbsolute (value : String) : Int;
};

export let join (left : String, right : String) : String := ask PathHost.join(left, right);
export let normalize (value : String) : String := ask PathHost.normalize(value);
export let dirname (value : String) : String := ask PathHost.dirname(value);
export let basename (value : String) : String := ask PathHost.basename(value);
export let extname (value : String) : String := ask PathHost.extname(value);
export let isAbsolute (value : String) : Int := ask PathHost.isAbsolute(value);
