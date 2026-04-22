let Core := import "musi:core";
let Int := Core.Int;
let String := Core.String;

export opaque let Env := effect {
  let get (name : String) : String;
  let has (name : String) : Int;
  let set (name : String, value : String) : Int;
  let remove (name : String) : Int;
};

export let get (name : String) : String := ask Env.get(name);
export let has (name : String) : Int := ask Env.has(name);
export let set (name : String, value : String) : Int := ask Env.set(name, value);
export let remove (name : String) : Int := ask Env.remove(name);
