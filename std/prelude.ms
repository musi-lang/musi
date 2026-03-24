// --- Primitive types - compiler erases to NaN-box tags at codegen. ---
/// Logical boolean with two variants: `False` and `True`.
export let Bool := choice {
    @_intrinsic(opcode := "ld.false")
    False
  | @_intrinsic(opcode := "ld.true")
    True
};
/// The unit type, representing absence of data.
export let Unit := record {};
/// The uninhabited type, used for non-terminating functions.
export let Never := choice {};

// --- Platform-sized numeric types ---
/// Platform-sized signed integer.
export opaque let Int := record {};
/// Platform-sized unsigned integer.
export opaque let Nat := record {};
/// Platform-sized floating-point number.
export opaque let Float := record {};

// --- Sized integers ---
/// 8-bit signed integer.
export opaque let Int8 := record {};
/// 16-bit signed integer.
export opaque let Int16 := record {};
/// 32-bit signed integer.
export opaque let Int32 := record {};
/// 64-bit signed integer.
export opaque let Int64 := record {};

// --- Sized unsigned integers ---
/// 8-bit unsigned integer.
export opaque let Nat8 := record {};
/// 16-bit unsigned integer.
export opaque let Nat16 := record {};
/// 32-bit unsigned integer.
export opaque let Nat32 := record {};
/// 64-bit unsigned integer.
export opaque let Nat64 := record {};

// --- Sized floats ---
/// 32-bit floating-point number.
export opaque let Float32 := record {};
/// 64-bit floating-point number.
export opaque let Float64 := record {};

// --- Text types ---
/// A single Unicode character.
export opaque let Rune := record {};
/// A sequence of byte values representing UTF-8 encoded text.
export let String := Array of Nat8;

// --- Collection types ---
/// A fixed-size array of elements of type `T`.
export opaque let Array [T] := record {};
/// A linked list of elements of type `T`.
export opaque let List [T] := record {};

// --- Universe types - compiler intrinsics, declared here for visibility. ---
/// The type of types.
export opaque let Type := record {};
/// A type that can represent any value (escape hatch for the type system).
export opaque let Any := record {};
/// A placeholder for unknown or yet-to-be-determined types.
export opaque let Unknown := record {};

// --- Core type classes ---
/// Equality type class for types that can be compared for equality.
export let Eq [T] := class {
    /// Equality operator: returns `True` if values are equal.
    let (=) (a : T, b : T) : Bool;
    /// Inequality operator: returns `True` if values are not equal.
    let (/=) (a : T, b : T) : Bool := not (a = b);
};

/// Ordering type class for types that have a total order.
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

/// Numeric type class for types that support arithmetic operations.
export let Num [T] := class where T : Eq {
    /// Addition operator.
    let (+) (a : T, b : T) : T;
    /// Subtraction operator.
    let (-) (a : T, b : T) : T;
    /// Multiplication operator.
    let (*) (a : T, b : T) : T;
    /// Division operator.
    let (/) (a : T, b : T) : T;
    /// Remainder operator.
    let (%) (a : T, b : T) : T;
    /// Negation operator.
    let neg (a : T) : T;
};

/// Type class for types that can be converted to a string representation.
export let Show [T] := class {
    /// Converts a value to its string representation.
    let show (a : T) : String;
};

/// Bitwise shift operations for integer types.
export let Bits [T] := class {
    @_intrinsic(opcode := "shl")
    let shl (a : T, n : Int) : T;
    @_intrinsic(opcode := "shr")
    let shr (a : T, n : Int) : T;
};
