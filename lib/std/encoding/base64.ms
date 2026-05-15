--! Base64 encoding helpers.

let maybe := import "@std/maybe";
let encodingHost := import "musi:encoding";
let Maybe := maybe.Maybe;

--- Encode a value into this format.
export let encode (source : String) := encodingHost.base64Encode(source);

--- Returns true when source is valid for this format.
export let isValid (source : String) := encodingHost.base64IsValid(source) > 0;

--- Decode a value from this format.
export let decode (source : String) :=
  if isValid(source) then maybe.Some[String](encodingHost.base64Decode(source))
  else maybe.None[String]();
