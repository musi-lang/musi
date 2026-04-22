let string := import "./string.ms";

--- Count Unicode scalar values in a string.
export let(self : String).length () : Int := string.length(self);

--- Returns true when string is empty.
export let(self : String).isEmpty () : Bool := string.isEmpty(self);

--- Trim surrounding whitespace.
export let(self : String).trim () : String := string.trim(self);

--- Concatenate strings.
export let(self : String).concat (other : String) : String :=
    string.concat(self, other);

--- Concatenate strings.
export let concat (left : String, right : String) : String :=
    string.concat(left, right);

--- Convert to lower case.
export let(self : String).toLowerCase () : String := string.toLowerCase(self);

--- Convert to upper case.
export let(self : String).toUpperCase () : String := string.toUpperCase(self);

--- Returns true when string contains substring.
export let(self : String).contains (needle : String) : Bool :=
    string.contains(self, needle);

--- Returns true when string starts with prefix.
export let(self : String).startsWith (prefix : String) : Bool :=
    string.startsWith(self, prefix);

--- Returns true when string ends with suffix.
export let(self : String).endsWith (suffix : String) : Bool :=
    string.endsWith(self, suffix);

--- Return first byte index for substring, or -1 when missing.
export let(self : String).indexOf (needle : String) : Int :=
    string.indexOf(self, needle);

--- Return byte-range string slice on character boundaries.
export let(self : String).slice (start : Int, end : Int) : String :=
    string.slice(self, start, end);

--- Return byte value at index, or -1 when missing.
export let byteAt (target : String, index : Int) : Int :=
    string.byteAt(target, index);
