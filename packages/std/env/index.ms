let option := import "@std/option";
let Option := option.Option;
let envHost := import "musi:env";

--- Environment variable key.
export opaque let Key := data {
  | Key(value : String)
};

--- Environment variable value.
export opaque let Var := data {
  | Var(value : String)
};

--- Build an environment key.
export let key (value : String) : Key := .Key(value := value);

--- Build an environment value.
export let var (value : String) : Var := .Var(value := value);

--- Return raw key text.
let keyText (target : Key) : String :=
  match target (
  | .Key(value := value) => value
  );

--- Convert value to string.
export let(self : Key).toString () : String := keyText(self);

--- Return raw variable text.
let varText (target : Var) : String :=
  match target (
  | .Var(value := value) => value
  );

--- Convert value to string.
export let(self : Var).toString () : String := varText(self);

--- Returns true when variable exists.
export let has (target : Key) : Bool := envHost.has(keyText(target)) > 0;

--- Read an environment variable.
export let get (target : Key) : Option[Var] :=
  match has(target) (
  | ok if ok => option.someOf[Var](var(envHost.get(keyText(target))))
  | _ => option.noneOf[Var]()
  );

--- Set an environment variable.
export let set (target : Key, value : Var) : Bool :=
  envHost.set(keyText(target), varText(value)) > 0;

--- Remove an environment variable.
export let remove (target : Key) : Bool :=
  envHost.remove(keyText(target)) > 0;
