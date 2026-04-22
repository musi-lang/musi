let core := import "./_core.ms";

--- Ordering result for comparisons.
export let Ordering := core.Ordering;

--- Equality contract.
export let Eq := core.Eq;

--- Ordering contract.
export let Ord := core.Ord;

--- Less-than ordering value.
export let less := core.less;

--- Equal ordering value.
export let equal := core.equal;

--- Greater-than ordering value.
export let greater := core.greater;

--- Generic equality with built-in equality semantics.
export let eq := core.eq;

--- Generic inequality with built-in equality semantics.
export let notEq := core.notEq;

--- Compare integers.
export let compareInt := core.compareInt;

--- Compare floats with deterministic total ordering, including NaN and signed zero.
export let compareFloat := core.compareFloat;

--- Compare strings by Unicode scalar lexicographic order.
export let compareString := core.compareString;

--- Minimum integer.
export let minInt := core.minInt;

--- Maximum integer.
export let maxInt := core.maxInt;

--- Clamp an integer into inclusive bounds.
export let clampInt := core.clampInt;

--- Integer equality given.
export let intEq := core.intEq;

--- Float equality given.
export let floatEq := core.floatEq;

--- String equality given.
export let stringEq := core.stringEq;

--- Boolean equality given.
export let boolEq := core.boolEq;

--- Integer ordering given.
export let intOrd := core.intOrd;

--- Float ordering given.
export let floatOrd := core.floatOrd;

--- String ordering given.
export let stringOrd := core.stringOrd;

--- Returns true when ordering is less.
export let(self : Ordering).isLess () : Bool := core.isLess(self);

--- Returns true when ordering is equal.
export let(self : Ordering).isEqual () : Bool := core.isEqual(self);

--- Returns true when ordering is greater.
export let(self : Ordering).isGreater () : Bool := core.isGreater(self);
