let runtime := import "musi:runtime";

--- Internal implementation for `length`.
export let length (target : String) : Int := runtime.textLength(target);

--- Internal implementation for `isEmpty`.
export let isEmpty (target : String) : Bool := length(target) = 0;

--- Internal implementation for `trim`.
export let trim (target : String) : String := runtime.textTrim(target);

--- Internal implementation for `toLowerCase`.
export let toLowerCase (target : String) : String :=
    runtime.textToLowerCase(target);

--- Internal implementation for `toUpperCase`.
export let toUpperCase (target : String) : String :=
    runtime.textToUpperCase(target);

--- Internal implementation for `contains`.
export let contains (target : String, needle : String) : Bool :=
    runtime.textContains(target, needle) > 0;

--- Internal implementation for `startsWith`.
export let startsWith (target : String, prefix : String) : Bool :=
    runtime.textStartsWith(target, prefix) > 0;

--- Internal implementation for `endsWith`.
export let endsWith (target : String, suffix : String) : Bool :=
    runtime.textEndsWith(target, suffix) > 0;
