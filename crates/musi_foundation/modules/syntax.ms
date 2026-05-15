--! Compiler syntax builtin module.

let Core := import "musi:core";
let Any := Core.Any;
let String := Core.String;
let Syntax := Core.Syntax;
let Type := Core.Type;
let Unit := Core.Unit;

@foreign(abi := .musi)
let Musi__eval (body : Syntax, result : Type) : Any;

--- Evaluate syntax as a value of expected type.
export let eval (body : Syntax, result : Type) :=
  unsafe (Musi__eval(body, result));

@foreign(abi := .musi)
let Musi__registerModule (spec : String, body : Syntax) : Unit;

--- Register module syntax under a specifier.
export let registerModule (spec : String, body : Syntax) :=
  unsafe (Musi__registerModule(spec, body));
