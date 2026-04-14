export opaque let Json := data {
  | Json : String
};

export let fromString (source : String) : Json := .Json(source);

export let toString (value : Json) : String :=
  match value (
  | .Json(source) => source
  );

export let isValid (source : String) : Bool := 0 = 0;

export let parse (source : String) : Json := fromString(source);

export let stringify (value : Json) : String := toString(value);
