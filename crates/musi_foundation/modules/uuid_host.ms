let Core := import "musi:core";
let String := Core.String;

export opaque let UuidHost := effect {
  let v4 () : String;
};

export let v4 () : String := ask UuidHost.v4();
