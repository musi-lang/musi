let textHost := import "musi:text";

--- Internal implementation for `length`.
export let length (target : String) : Int := textHost.length(target);

--- Internal implementation for `isEmpty`.
export let isEmpty (target : String) : Bool := length(target) = 0;

--- Internal implementation for `trim`.
export let trim (target : String) : String := textHost.trim(target);

--- Internal implementation for `concat`.
export let concat (left : String, right : String) : String :=
    textHost.concat(left, right);

--- Internal implementation for `toLowerCase`.
export let toLowerCase (target : String) : String :=
    textHost.toLowerCase(target);

--- Internal implementation for `toUpperCase`.
export let toUpperCase (target : String) : String :=
    textHost.toUpperCase(target);

--- Internal implementation for `contains`.
export let contains (target : String, needle : String) : Bool :=
    textHost.contains(target, needle) > 0;

--- Internal implementation for `startsWith`.
export let startsWith (target : String, prefix : String) : Bool :=
    textHost.startsWith(target, prefix) > 0;

--- Internal implementation for `endsWith`.
export let endsWith (target : String, suffix : String) : Bool :=
    textHost.endsWith(target, suffix) > 0;

--- Internal implementation for `indexOf`.
export let indexOf (target : String, needle : String) : Int :=
    textHost.indexOf(target, needle);

--- Internal implementation for `slice`.
export let slice (target : String, start : Int, end : Int) : String :=
    textHost.slice(target, start, end);

--- Internal implementation for `byteAt`.
export let byteAt (target : String, index : Int) : Int :=
    textHost.byteAt(target, index);
