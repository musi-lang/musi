let runtime := import "musi:runtime";

/// Normalized JSON text.
export opaque let Json := data {
  | Json(source : String)
};

/// Parse and normalize JSON text.
export let parse (source : String) : Json := .Json(source := runtime.jsonNormalize(source));

/// Render JSON text.
export let (self : Json).stringify () : String :=
  match self (
  | .Json(source := source) => source
  );

/// Returns true when source is valid JSON.
export let isValid (source : String) : Bool := runtime.jsonIsValid(source) > 0;
