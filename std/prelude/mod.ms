// --- Primitive types - compiler erases to NaN-box tags at codegen. ---
/// `Bool` - logical boolean with two variants: `False` and `True`.
export let Bool := choice { False | True };
/// `Unit` - the unit type, representing absence of data.
export let Unit := record {};
/// `Never` - the uninhabited type, used for non-terminating functions.
export let Never := choice {};

// --- Platform-sized numeric types ---
/// `Int` - platform-sized signed integer.
export opaque let Int := record {};
/// `Nat` - platform-sized unsigned integer.
export opaque let Nat := record {};
/// `Float` - platform-sized floating-point number.
export opaque let Float := record {};

// --- Sized integers ---
/// `Int8` - 8-bit signed integer.
export opaque let Int8 := record {};
/// `Int16` - 16-bit signed integer.
export opaque let Int16 := record {};
/// `Int32` - 32-bit signed integer.
export opaque let Int32 := record {};
/// `Int64` - 64-bit signed integer.
export opaque let Int64 := record {};

// --- Sized unsigned integers ---
/// `Nat8` - 8-bit unsigned integer.
export opaque let Nat8 := record {};
/// `Nat16` - 16-bit unsigned integer.
export opaque let Nat16 := record {};
/// `Nat32` - 32-bit unsigned integer.
export opaque let Nat32 := record {};
/// `Nat64` - 64-bit unsigned integer.
export opaque let Nat64 := record {};

// --- Sized floats ---
/// `Float32` - 32-bit floating-point number.
export opaque let Float32 := record {};
/// `Float64` - 64-bit floating-point number.
export opaque let Float64 := record {};

// --- Text types ---
/// `Rune` - a single Unicode character.
export opaque let Rune := record {};
/// `String` - a sequence of byte values representing UTF-8 encoded text.
export let String := Array of Nat8;

// --- Collection types ---
/// `Array [T]` - a fixed-size array of elements of type `T`.
export opaque let Array [T] := record {};
/// `List [T]` - a linked list of elements of type `T`.
export opaque let List [T] := record {};

// --- Universe types - compiler intrinsics, declared here for visibility. ---
/// `Type` - the type of types.
export opaque let Type := record {};
/// `Any` - a type that can represent any value (escape hatch for the type system).
export opaque let Any := record {};
/// `Unknown` - a placeholder for unknown or yet-to-be-determined types.
export opaque let Unknown := record {};

// --- Core type classes ---
/// `Eq [T]` - equality type class for types that can be compared for equality.
export let Eq [T] := class {
    /// Equality operator: returns `True` if values are equal.
    let (=) (a : T, b : T) : Bool;
    /// Inequality operator: returns `True` if values are not equal.
    let (/=) (a : T, b : T) : Bool := not (a = b);
};

/// `Ord [T]` - ordering type class for types that have a total order.
export let Ord [T] := class where T : Eq {
    /// Less-than operator.
    let (<) (a : T, b : T) : Bool;
    /// Greater-than operator.
    let (>) (a : T, b : T) : Bool;
    /// Less-than-or-equal operator.
    let (<=) (a : T, b : T) : Bool;
    /// Greater-than-or-equal operator.
    let (>=) (a : T, b : T) : Bool;
};

/// `Num [T]` - numeric type class for types that support arithmetic operations.
export let Num [T] := class where T : Eq {
    /// Addition operator.
    let (+) (a : T, b : T) : T;
    /// Subtraction operator.
    let (-) (a : T, b : T) : T;
    /// Multiplication operator.
    let (*) (a : T, b : T) : T;
    /// Division operator.
    let (/) (a : T, b : T) : T;
    /// Modulo operator.
    let (%) (a : T, b : T) : T;
    /// Negation operator.
    let neg (a : T) : T;
};

/// `Show [T]` - type class for types that can be converted to a string representation.
export let Show [T] := class {
    /// Converts a value to its string representation.
    let show (a : T) : String;
};
