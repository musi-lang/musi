let Runtime := import "musi:runtime";

export opaque let Json := data {
  | Json(String)
};

export let parse (source : String) : Json := .Json(Runtime.jsonNormalize(source));

export let stringify (value : Json) : String :=
  match value (
  | .Json(source) => source
  );

export let isValid (source : String) : Bool := Runtime.jsonIsValid(source) > 0;
