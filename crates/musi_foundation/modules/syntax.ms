let Core := import "musi:core";
let Any := Core.Any;
let String := Core.String;
let Syntax := Core.Syntax;
let Type := Core.Type;
let Unit := Core.Unit;

export opaque let SyntaxOps := effect {
  let eval (body : Syntax, result : Type) : Any;
  let registerModule (spec : String, body : Syntax) : Unit;
};

export let eval (body : Syntax, result : Type) : Any :=
  ask SyntaxOps.eval(body, result);

export let registerModule (spec : String, body : Syntax) : Unit :=
  ask SyntaxOps.registerModule(spec, body);
