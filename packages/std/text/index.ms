let Runtime := import "musi:runtime";

export let length (value : String) : Int := Runtime.textLength(value);

export let isEmpty (value : String) : Bool := length(value) = 0;

export let trim (value : String) : String := Runtime.textTrim(value);

export let toLowerCase (value : String) : String := Runtime.textToLowerCase(value);

export let toUpperCase (value : String) : String := Runtime.textToUpperCase(value);

export let contains (value : String, needle : String) : Bool := Runtime.textContains(value, needle) > 0;

export let startsWith (value : String, prefix : String) : Bool := Runtime.textStartsWith(value, prefix) > 0;

export let endsWith (value : String, suffix : String) : Bool := Runtime.textEndsWith(value, suffix) > 0;
