let runtime := import "musi:runtime";

export let length (value : String) : Int := runtime.textLength(value);

export let isEmpty (value : String) : Bool := length(value) = 0;

export let trim (value : String) : String := runtime.textTrim(value);

export let toLowerCase (value : String) : String := runtime.textToLowerCase(value);

export let toUpperCase (value : String) : String := runtime.textToUpperCase(value);

export let contains (value : String, needle : String) : Bool := runtime.textContains(value, needle) > 0;

export let startsWith (value : String, prefix : String) : Bool := runtime.textStartsWith(value, prefix) > 0;

export let endsWith (value : String, suffix : String) : Bool := runtime.textEndsWith(value, suffix) > 0;
