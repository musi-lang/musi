let textHost := import "musi:text";

--- Return byte length.
let textLength (target : String) : Int := textHost.length(target);

--- Return byte length.
export let length (target : String) : Int := textLength(target);

--- Core operation for `isEmpty`.
let textIsEmpty (target : String) : Bool := textLength(target) = 0;

--- Core operation for `isEmpty`.
export let isEmpty (target : String) : Bool := textIsEmpty(target);

--- Concatenate text.
let textConcat (left : String, right : String) : String :=
  textHost.concat(left, right);

--- Concatenate text.
export let concat (left : String, right : String) : String :=
  textConcat(left, right);

--- Return byte-range text slice on character boundaries.
let textSlice (target : String, start : Int, end : Int) : String :=
  textHost.slice(target, start, end);

--- Return byte-range text slice on character boundaries.
export let slice (target : String, start : Int, end : Int) : String :=
  textSlice(target, start, end);

--- Return byte value at index.
let textByteAt (target : String, index : Int) : Int :=
  textHost.byteAt(target, index);

--- Return byte value at index.
export let byteAt (target : String, index : Int) : Int :=
  textByteAt(target, index);

--- Build one-byte text from byte value.
let textFromByte (value : Int) : String := textHost.fromByte(value);

--- Build one-byte text from byte value.
export let fromByte (value : Int) : String := textFromByte(value);

let both (left : Bool, right : Bool) : Bool :=
  match left (
  | ok if ok => right
  | _ => 0 = 1
  );

let isWhitespaceByte (value : Int) : Bool :=
  match value (
  | 9 => 0 = 0
  | 10 => 0 = 0
  | 13 => 0 = 0
  | 32 => 0 = 0
  | _ => 0 = 1
  );

let rec trimStartIndex (target : String, index : Int, end : Int) : Int :=
  match () (
  | _ if index >= end => end
  | _ if isWhitespaceByte(textByteAt(target, index)) => trimStartIndex(
    target,
    index + 1,
    end,
  )
  | _ => index
  );

let rec trimEndIndex (target : String, start : Int, end : Int) : Int :=
  match () (
  | _ if end <= start => start
  | _ if isWhitespaceByte(textByteAt(target, end - 1)) => trimEndIndex(
    target,
    start,
    end - 1,
  )
  | _ => end
  );

--- Core operation for `trim`.
let textTrim (target : String) : String :=
  (
    let end := textLength(target);
    let start := trimStartIndex(target, 0, end);
    textSlice(target, start, trimEndIndex(target, start, end))
  );

--- Core operation for `trim`.
export let trim (target : String) : String := textTrim(target);

let lowerByte (value : Int) : Int :=
  match () (
  | _ if both(value >= 65, value <= 90) => value + 32
  | _ => value
  );

let upperByte (value : Int) : Int :=
  match () (
  | _ if both(value >= 97, value <= 122) => value - 32
  | _ => value
  );

let rec transformAscii (
  target : String,
  index : Int,
  end : Int,
  mapper : Int -> Int
) : String :=
  match () (
  | _ if index >= end => ""
  | _ => textConcat(
    textFromByte(mapper(textByteAt(target, index))),
    transformAscii(target, index + 1, end, mapper),
  )
  );

--- Core operation for `toLowerCase`.
let textToLowerCase (target : String) : String :=
  transformAscii(target, 0, textLength(target), lowerByte);

--- Core operation for `toLowerCase`.
export let toLowerCase (target : String) : String := textToLowerCase(target);

--- Core operation for `toUpperCase`.
let textToUpperCase (target : String) : String :=
  transformAscii(target, 0, textLength(target), upperByte);

--- Core operation for `toUpperCase`.
export let toUpperCase (target : String) : String := textToUpperCase(target);

let bytesDifferAt (
  target : String,
  needle : String,
  targetIndex : Int,
  needleIndex : Int
) : Bool := textByteAt(target, targetIndex) /= textByteAt(needle, needleIndex);

let rec matchesAt (
  target : String,
  needle : String,
  targetIndex : Int,
  needleIndex : Int
) : Bool :=
  match () (
  | _ if needleIndex >= textLength(needle) => 0 = 0
  | _ if targetIndex >= textLength(target) => 0 = 1
  | _ if bytesDifferAt(target, needle, targetIndex, needleIndex) => 0 = 1
  | _ => matchesAt(target, needle, targetIndex + 1, needleIndex + 1)
  );

let rec indexOfFrom (
  target : String,
  needle : String,
  index : Int
) : Int :=
  match () (
  | _ if textLength(needle) = 0 => index
  | _ if index > textLength(target) - textLength(needle) => - 1
  | _ if matchesAt(target, needle, index, 0) => index
  | _ => indexOfFrom(target, needle, index + 1)
  );

--- Core operation for `indexOf`.
let textIndexOf (target : String, needle : String) : Int :=
  indexOfFrom(target, needle, 0);

--- Core operation for `indexOf`.
export let indexOf (target : String, needle : String) : Int :=
  textIndexOf(target, needle);

--- Core operation for `contains`.
let textContains (target : String, needle : String) : Bool :=
  textIndexOf(target, needle) >= 0;

--- Core operation for `contains`.
export let contains (target : String, needle : String) : Bool :=
  textContains(target, needle);

--- Core operation for `startsWith`.
let textStartsWith (target : String, prefix : String) : Bool :=
  matchesAt(target, prefix, 0, 0);

--- Core operation for `startsWith`.
export let startsWith (target : String, prefix : String) : Bool :=
  textStartsWith(target, prefix);

--- Core operation for `endsWith`.
let textEndsWith (target : String, suffix : String) : Bool :=
  match () (
  | _ if textLength(suffix) > textLength(target) => 0 = 1
  | _ => matchesAt(target, suffix, textLength(target) - textLength(suffix), 0)
  );

--- Core operation for `endsWith`.
export let endsWith (target : String, suffix : String) : Bool :=
  textEndsWith(target, suffix);

--- Count Unicode scalar values in string.
export let (self : String).length () : Int := textLength(self);

--- Returns true when string is empty.
export let (self : String).isEmpty () : Bool := textIsEmpty(self);

--- Trim surrounding whitespace.
export let (self : String).trim () : String := textTrim(self);

--- Concatenate strings.
export let (self : String).concat (other : String) : String :=
  textConcat(self, other);

--- Convert to lower case.
export let (self : String).toLowerCase () : String := textToLowerCase(self);

--- Convert to upper case.
export let (self : String).toUpperCase () : String := textToUpperCase(self);

--- Returns true when string contains substring.
export let (self : String).contains (needle : String) : Bool :=
  textContains(self, needle);

--- Returns true when string starts with prefix.
export let (self : String).startsWith (prefix : String) : Bool :=
  textStartsWith(self, prefix);

--- Returns true when string ends with suffix.
export let (self : String).endsWith (suffix : String) : Bool :=
  textEndsWith(self, suffix);

--- Return first byte index for substring, or -1 when missing.
export let (self : String).indexOf (needle : String) : Int :=
  textIndexOf(self, needle);

--- Return byte-range string slice on character boundaries.
export let (self : String).slice (start : Int, end : Int) : String :=
  textSlice(self, start, end);
