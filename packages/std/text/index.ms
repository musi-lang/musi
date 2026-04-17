let runtime := import "musi:runtime";

/// Count Unicode scalar values in a string.
export let (self : String).length () : Int := runtime.textLength(self);

/// Returns true when string is empty.
export let (self : String).isEmpty () : Bool := length(self) = 0;

/// Trim surrounding whitespace.
export let (self : String).trim () : String := runtime.textTrim(self);

/// Convert to lower case.
export let (self : String).toLowerCase () : String := runtime.textToLowerCase(self);

/// Convert to upper case.
export let (self : String).toUpperCase () : String := runtime.textToUpperCase(self);

/// Returns true when string contains substring.
export let (self : String).contains (needle : String) : Bool := runtime.textContains(self, needle) > 0;

/// Returns true when string starts with prefix.
export let (self : String).startsWith (prefix : String) : Bool := runtime.textStartsWith(self, prefix) > 0;

/// Returns true when string ends with suffix.
export let (self : String).endsWith (suffix : String) : Bool := runtime.textEndsWith(self, suffix) > 0;
