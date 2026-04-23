let option := import "@std/option";
let encodingHost := import "musi:encoding";
let Option := option.Option;

--- Encode a value into this format.
export let encode (source : String) : String := encodingHost.utf8Encode(source);

--- Returns true when source is valid for this format.
export let isValid (bytes : String) : Bool :=
  encodingHost.utf8IsValid(bytes) > 0;

--- Decode a value from this format.
export let decode (bytes : String) : Option[String] :=
  match isValid(bytes) (
  | valid if valid => option.someOf[String](encodingHost.utf8Decode(bytes))
  | _ => option.noneOf[String]()
  );
