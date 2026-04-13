let Option := import "@std/option";
let Runtime := import "musi:runtime";

export let has (key : String) : Bool := Runtime.envHas(key) > 0;

export let get (key : String) :=
  case has(key) of (
  | ok if ok => Option.some[String](Runtime.envGet(key))
  | _ => Option.none[String]()
  );

export let set (key : String, value : String) : Bool := Runtime.envSet(key, value) > 0;

export let remove (key : String) : Bool := Runtime.envRemove(key) > 0;
