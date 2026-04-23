let textHost := import "musi:text";

--- Return byte length.
export let length (target : String) : Int := textHost.length(target);

--- Core operation for `isEmpty`.
export let isEmpty (target : String) : Bool := length(target) = 0;

--- Concatenate text.
export let concat (left : String, right : String) : String :=
  textHost.concat(left, right);

--- Return byte-range text slice on character boundaries.
export let slice (target : String, start : Int, end : Int) : String :=
  textHost.slice(target, start, end);

--- Return byte value at index.
export let byteAt (target : String, index : Int) : Int :=
  textHost.byteAt(target, index);

--- Build one-byte text from byte value.
export let fromByte (value : Int) : String := textHost.fromByte(value);

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
  | _ if isWhitespaceByte(byteAt(target, index)) => trimStartIndex(
    target,
    index + 1,
    end,
  )
  | _ => index
  );

let rec trimEndIndex (target : String, start : Int, end : Int) : Int :=
  match () (
  | _ if end <= start => start
  | _ if isWhitespaceByte(byteAt(target, end - 1)) => trimEndIndex(
    target,
    start,
    end - 1,
  )
  | _ => end
  );

--- Core operation for `trim`.
export let trim (target : String) : String :=
  (
    let end := length(target);
    let start := trimStartIndex(target, 0, end);
    slice(target, start, trimEndIndex(target, start, end))
  );

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
  | _ => concat(
    fromByte(mapper(byteAt(target, index))),
    transformAscii(target, index + 1, end, mapper),
  )
  );

--- Core operation for `toLowerCase`.
export let toLowerCase (target : String) : String :=
  transformAscii(target, 0, length(target), lowerByte);

--- Core operation for `toUpperCase`.
export let toUpperCase (target : String) : String :=
  transformAscii(target, 0, length(target), upperByte);

let rec matchesAt (
  target : String,
  needle : String,
  targetIndex : Int,
  needleIndex : Int
) : Bool :=
  match () (
  | _ if needleIndex >= length(needle) => 0 = 0
  | _ if targetIndex >= length(target) => 0 = 1
  | _ if byteAt(target, targetIndex) /= byteAt(needle, needleIndex) => 0 = 1
  | _ => matchesAt(target, needle, targetIndex + 1, needleIndex + 1)
  );

let rec indexOfFrom (
  target : String,
  needle : String,
  index : Int
) : Int :=
  match () (
  | _ if length(needle) = 0 => index
  | _ if index > length(target) - length(needle) => - 1
  | _ if matchesAt(target, needle, index, 0) => index
  | _ => indexOfFrom(target, needle, index + 1)
  );

--- Core operation for `indexOf`.
export let indexOf (target : String, needle : String) : Int :=
  indexOfFrom(target, needle, 0);

--- Core operation for `contains`.
export let contains (target : String, needle : String) : Bool :=
  indexOf(target, needle) >= 0;

--- Core operation for `startsWith`.
export let startsWith (target : String, prefix : String) : Bool :=
  matchesAt(target, prefix, 0, 0);

--- Core operation for `endsWith`.
export let endsWith (target : String, suffix : String) : Bool :=
  match () (
  | _ if length(suffix) > length(target) => 0 = 1
  | _ => matchesAt(target, suffix, length(target) - length(suffix), 0)
  );

--- Count Unicode scalar values in string.
export let (self : String).length () : Int := length(self);

--- Returns true when string is empty.
export let (self : String).isEmpty () : Bool := isEmpty(self);

--- Trim surrounding whitespace.
export let (self : String).trim () : String := trim(self);

--- Concatenate strings.
export let (self : String).concat (other : String) : String :=
  concat(self, other);

--- Convert to lower case.
export let (self : String).toLowerCase () : String := toLowerCase(self);

--- Convert to upper case.
export let (self : String).toUpperCase () : String := toUpperCase(self);

--- Returns true when string contains substring.
export let (self : String).contains (needle : String) : Bool :=
  contains(self, needle);

--- Returns true when string starts with prefix.
export let (self : String).startsWith (prefix : String) : Bool :=
  startsWith(self, prefix);

--- Returns true when string ends with suffix.
export let (self : String).endsWith (suffix : String) : Bool :=
  endsWith(self, suffix);

--- Return first byte index for substring, or -1 when missing.
export let (self : String).indexOf (needle : String) : Int :=
  indexOf(self, needle);

--- Return byte-range string slice on character boundaries.
export let (self : String).slice (start : Int, end : Int) : String :=
  slice(self, start, end);
