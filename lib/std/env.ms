--! Environment variable helpers.

let maybe := import "@std/maybe";
let Maybe := maybe.Maybe;
let envHost := import "musi:env";

--- Environment variable key.
export hidden let Key := data {
  | EnvKey(value : String)
};

--- Environment variable value.
export hidden let Var := data {
  | EnvVar(value : String)
};

--- Build an environment key.
export let key (value : String) := .EnvKey(value := value);

--- Build an environment value.
export let var (value : String) := .EnvVar(value := value);

--- Return raw key text.
let keyText (target : Key) := match target (
| .EnvKey(value := value) => value
);

--- Convert value to string.
export let (self : Key).toString () := keyText(self);

--- Return raw variable text.
let varText (target : Var) := match target (
| .EnvVar(value := value) => value
);

--- Convert value to string.
export let (self : Var).toString () := varText(self);

--- Returns true when variable exists.
export let has (target : Key) := envHost.has(keyText(target)) > 0;

--- Read an environment variable.
export let get (target : Key) :=
  if has(target) then maybe.Some[Var](var(envHost.get(keyText(target))))
  else maybe.None[Var]();

--- Set an environment variable.
export let set (target : Key, value : Var) :=
  envHost.set(keyText(target), varText(value)) > 0;

--- Remove an environment variable.
export let remove (target : Key) := envHost.remove(keyText(target)) > 0;
