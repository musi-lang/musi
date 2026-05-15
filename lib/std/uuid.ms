--! UUID module.

let text := import "@std/text";
let uuidHost := import "musi:uuid";

--- UUID text value.
export hidden let Uuid := data {
  | UuidText(value : String)
};

--- Generate a random version-4 UUID.
export let v4 () := .UuidText(value := uuidHost.v4());

--- Parse UUID text.
export let parse (source : String) := .UuidText(value := source);

let isHyphenAt (source : String, index : Int) :=
  text.byteAt(source, index) = 45;

let inRange (value : Int, lower : Int, upper : Int) :=
  value >= lower and value <= upper;

let isHexByte (value : Int) :=
  inRange(value, 48, 57) or inRange(value, 65, 70) or inRange(value, 97, 102);

let isUuidHexAt (source : String, index : Int) :=
  isHexByte(text.byteAt(source, index));

let recur isUuidHexSpan (source : String, index : Int, end : Int) :=
  if index >= end then 0 = 0
  else if isUuidHexAt(source, index) then isUuidHexSpan(source, index + 1, end)
  else 0 = 1;

let hasUuidHyphens (source : String) :=
  (
    let first := isHyphenAt(source, 8);
    let second := isHyphenAt(source, 13);
    let third := isHyphenAt(source, 18);
    let fourth := isHyphenAt(source, 23);
    first and second and third and fourth
  );

let isUuidTextValid (source : String) :=
  (
    let first := source |> isUuidHexSpan(0, 8);
    let second := source |> isUuidHexSpan(9, 13);
    let third := source |> isUuidHexSpan(14, 18);
    let fourth := source |> isUuidHexSpan(19, 23);
    let fifth := source |> isUuidHexSpan(24, 36);
    first and second and third and fourth and fifth
  );

--- Returns true when text is a valid UUID.
export let isValid (source : String) :=
  (
    let hasLength := source.length() = 36;
    let hasHyphens := source |> hasUuidHyphens();
    let hasHex := source |> isUuidTextValid();
    hasLength and hasHyphens and hasHex
  );

--- Return canonical UUID text.
export let (self : Uuid).toString () :=
  match self (
  | .UuidText(value := value) => value
  );
