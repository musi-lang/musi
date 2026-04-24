let jsonHost := import "musi:json";

--- Normalized JSON text.
export opaque let Json := data {
  | JsonSource(source : String)
};

--- Parse and normalize JSON text.
export let parse (source : String) : Json :=
  .JsonSource(source := jsonHost.normalize(source));

--- Render JSON text.
export let (self : Json).stringify () : String :=
  match self (
  | .JsonSource(source := source) => source
  );

--- Returns true when source is valid JSON.
export let isValid (source : String) : Bool := jsonHost.isValid(source) > 0;
