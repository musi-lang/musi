let option := import "@std/option";
let runtime := import "musi:runtime";
let Option := option.Option;

/// Encode a value into this format.
export let encode (source : String) : String := runtime.utf8Encode(source);

/// Returns true when source is valid for this format.
export let isValid (bytes : String) : Bool := runtime.utf8IsValid(bytes) > 0;

/// Decode a value from this format.
export let decode (bytes : String) : Option[String] :=
  match isValid(bytes) (
  | valid if valid => option.some[String](runtime.utf8Decode(bytes))
  | _ => option.none[String]()
  );
