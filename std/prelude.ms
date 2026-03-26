// --- Primitive types - compiler erases to NaN-box tags at codegen. ---
/// Logical boolean with two variants: `False` and `True`.
@builtin
export let Bool := data {
  | @builtin(opcode := 0x06)
    False
  | @builtin(opcode := 0x05)
    True
};
/// The unit type, representing absence of data.
@builtin
export let Unit := data { ; };
/// The uninhabited type, used for non-terminating functions.
@builtin
export let Empty := data { | };

// --- Platform-sized numeric types ---
/// Platform-sized signed integer.
@builtin
export opaque let Int := data { ; };
/// Platform-sized unsigned integer.
@builtin
export opaque let Nat := data { ; };
/// Platform-sized floating-point number.
@builtin
export opaque let Float := data { ; };

// --- Sized integers ---
/// 8-bit signed integer.
@builtin
export opaque let Int8 := data { ; };
/// 16-bit signed integer.
@builtin
export opaque let Int16 := data { ; };
/// 32-bit signed integer.
@builtin
export opaque let Int32 := data { ; };
/// 64-bit signed integer.
@builtin
export opaque let Int64 := data { ; };

// --- Sized unsigned integers ---
/// 8-bit unsigned integer.
@builtin
export opaque let Nat8 := data { ; };
/// 16-bit unsigned integer.
@builtin
export opaque let Nat16 := data { ; };
/// 32-bit unsigned integer.
@builtin
export opaque let Nat32 := data { ; };
/// 64-bit unsigned integer.
@builtin
export opaque let Nat64 := data { ; };

// --- Sized floats ---
/// 32-bit floating-point number.
@builtin
export opaque let Float32 := data { ; };
/// 64-bit floating-point number.
@builtin
export opaque let Float64 := data { ; };

// --- Text types ---
/// A single Unicode character.
@builtin
export opaque let Rune := data { ; };
/// A sequence of byte values representing UTF-8 encoded text.
export let String := Array of Nat8;

// --- Collection types ---
/// A fixed-size array of elements of type `T`.
@builtin
export opaque let Array [T] := data { ; };
/// A linked list of elements of type `T`.
@builtin
export opaque let List [T] := data { ; };

// --- Universe types - compiler intrinsics, declared here for visibility. ---
/// The type of types.
@builtin
export opaque let Type := data { ; };
/// A type that can represent any value (escape hatch for the type system).
@builtin
export opaque let Any := data { ; };
/// A placeholder for unknown or yet-to-be-determined types.
@builtin
export opaque let Unknown := data { ; };

// --- Core type classes ---
/// Equality type class for types that can be compared for equality.
export let Eq [T] := class {
    /// Equality operator: returns `True` if values are equal.
    @builtin(opcode := 0x25)  // CmpEq
    let (_ = _) (a : T, b : T) : Bool;
    /// Inequality operator: returns `True` if values are not equal.
    let (_ /= _) (a : T, b : T) : Bool := not (a = b);
};

/// Ordering type class for types that have a total order.
export let Ord [T] := class where T : Eq {
    /// Less-than operator.
    @builtin(opcode := 0x27)  // CmpLt
    let (_ < _) (a : T, b : T) : Bool;
    /// Greater-than operator.
    @builtin(opcode := 0x28)  // CmpGt
    let (_ > _) (a : T, b : T) : Bool;
    /// Less-than-or-equal operator.
    @builtin(opcode := 0x29)  // CmpLeq
    let (_ <= _) (a : T, b : T) : Bool;
    /// Greater-than-or-equal operator.
    @builtin(opcode := 0x2A)  // CmpGeq
    let (_ >= _) (a : T, b : T) : Bool;
};

/// Numeric type class for types that support arithmetic operations.
export let Num [T] := class where T : Eq {
    /// Addition operator.
    @builtin(opcode := 0x14)  // IAdd
    let (_ + _) (a : T, b : T) : T;
    /// Subtraction operator.
    @builtin(opcode := 0x15)  // ISub
    let (_ - _) (a : T, b : T) : T;
    /// Multiplication operator.
    @builtin(opcode := 0x16)  // IMul
    let (_ * _) (a : T, b : T) : T;
    /// Division operator.
    @builtin(opcode := 0x17)  // IDiv
    let (_ / _) (a : T, b : T) : T;
    /// Remainder operator.
    @builtin(opcode := 0x18)  // IRem
    let (_ % _) (a : T, b : T) : T;
    /// Negation operator.
    @builtin(opcode := 0x19)  // INeg
    let (- _) (a : T) : T;
};

/// Type class for types that can be converted to a string representation.
export let Show [T] := class {
    /// Converts a value to its string representation.
    let show (a : T) : String;
};

/// Bitwise shift operations for integer types.
export let Bits [T] := class {
    @builtin(opcode := 0x23)  // Shl
    let shl (a : T, n : Int) : T;
    @builtin(opcode := 0x24)  // Shr
    let shr (a : T, n : Int) : T;
};
