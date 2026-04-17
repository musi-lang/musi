let option := import "@std/option";
let runtime := import "musi:runtime";
let Option := option.Option;

/// Encode a value into this format.
export let encode (source : String) : String := runtime.base64Encode(source);

/// Returns true when source is valid for this format.
export let isValid (source : String) : Bool := runtime.base64IsValid(source) > 0;

/// Decode a value from this format.
export let decode (source : String) : Option[String] :=
  match isValid(source) (
  | valid if valid => option.some[String](runtime.base64Decode(source))
  | _ => option.none[String]()
  );
