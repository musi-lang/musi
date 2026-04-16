let option := import "@std/option";
let runtime := import "musi:runtime";

export let has (key : String) : Bool := runtime.envHas(key) > 0;

export let get (key : String) :=
  match has(key) (
  | ok if ok => option.some[String](runtime.envGet(key))
  | _ => option.none[String]()
  );

export let set (key : String, value : String) : Bool := runtime.envSet(key, value) > 0;

export let remove (key : String) : Bool := runtime.envRemove(key) > 0;
