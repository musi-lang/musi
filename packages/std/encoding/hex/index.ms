let option := import "@std/option";
let encodingHost := import "musi:encoding";
let Option := option.Option;

--- Encode a value into this format.
export let encode (source : String) : String := encodingHost.hexEncode(source);

--- Returns true when source is valid for this format.
export let isValid (source : String) : Bool := encodingHost.hexIsValid(source) > 0;

--- Decode a value from this format.
export let decode (source : String) : Option[String] :=
  match isValid(source) (
  | valid if valid => option.someOf[String](encodingHost.hexDecode(source))
  | _ => option.noneOf[String]()
  );
