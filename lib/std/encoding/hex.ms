--! Hexadecimal encoding helpers.

let maybe := import "@std/maybe";
let both (left : Bit, right : Bit) := if left then right else 0 = 1;

let text := import "@std/text";
let Maybe := maybe.Maybe;

let hexDigit (value : Int) :=
  match value (
  | 0 => "0"
  | 1 => "1"
  | 2 => "2"
  | 3 => "3"
  | 4 => "4"
  | 5 => "5"
  | 6 => "6"
  | 7 => "7"
  | 8 => "8"
  | 9 => "9"
  | 10 => "a"
  | 11 => "b"
  | 12 => "c"
  | 13 => "d"
  | 14 => "e"
  | _ => "f"
  );

let recur encodeFrom (source : String, index : Int, end : Int) :=
  if index >= end then ""
  else
  (
    let value := text.byteAt(source, index);
    text.concat(
      text.concat(hexDigit(value / 16), hexDigit(value % 16)),
      encodeFrom(source, index + 1, end),
    )
  );

--- Encode a value into this format.
export let encode (source : String) := encodeFrom(source, 0, source.length());

let hexValue (value : Int) :=
  if both(value >= 48, value <= 57) then value - 48
  else if both(value >= 65, value <= 70,) then value - 55
  else if both(value >= 97, value <= 102) then value - 87 else - 1;

let recur isValidFrom (source : String, index : Int, end : Int) :=
  if index >= end then 0 = 0
  else if hexValue(text.byteAt(source, index)) < 0 then 0 = 1
  else isValidFrom(source, index + 1, end);

--- Returns true when source is valid for this format.
export let isValid (source : String) :=
  if source.length() % 2 /= 0 then 0 = 1
  else isValidFrom(source, 0, source.length(),);

let recur decodeFrom (source : String, index : Int, end : Int) :=
  if index >= end then ""
  else
  (
    let hi := hexValue(text.byteAt(source, index));
    let lo := hexValue(text.byteAt(source, index + 1));
    text.concat(text.fromByte(hi * 16 + lo), decodeFrom(source, index + 2, end))
  );

--- Decode a value from this format.
export let decode (source : String) :=
  if isValid(source) then maybe.Some[String](
    decodeFrom(source, 0, source.length())
  ) else maybe.None[String]();
