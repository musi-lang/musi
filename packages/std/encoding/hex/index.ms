let option := import "@std/option";
let runtime := import "musi:runtime";
let Option := option.Option;

/// Encode a value into this format.
export let encode (source : String) : String := runtime.hexEncode(source);

/// Returns true when source is valid for this format.
export let isValid (source : String) : Bool := runtime.hexIsValid(source) > 0;

/// Decode a value from this format.
export let decode (source : String) : Option[String] :=
  match isValid(source) (
  | valid if valid => option.some[String](runtime.hexDecode(source))
  | _ => option.none[String]()
  );
