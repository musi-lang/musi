let string := import "./_string.ms";

--- Count Unicode scalar values in a string.
export let(self : String).length () : Int := string.length(self);

--- Returns true when string is empty.
export let(self : String).isEmpty () : Bool := string.isEmpty(self);

--- Trim surrounding whitespace.
export let(self : String).trim () : String := string.trim(self);

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
