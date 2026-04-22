let Core := import "musi:core";
let Int := Core.Int;
let String := Core.String;

export opaque let JsonHost := effect {
  let isValid (source : String) : Int;
  let normalize (source : String) : String;
};

export let isValid (source : String) : Int := ask JsonHost.isValid(source);
export let normalize (source : String) : String := ask JsonHost.normalize(source);
