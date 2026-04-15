export opaque let Json := data {
  | Json(String)
};

export let parse (source : String) : Json := .Json(source);

export let stringify (value : Json) : String :=
  match value (
  | .Json(source) => source
  );

export let isValid (source : String) : Bool := 0 = 0;
