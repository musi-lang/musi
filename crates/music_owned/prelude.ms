// --- Primitive types - compiler erases to NaN-box tags at codegen. ---
/// Logical boolean with two variants: `False` and `True`.
@musi.lang(name := "Bool")
export let Bool := data {
  | @musi.variant(opcode := 0x06, tag := 0, arity := 0)
    False
  | @musi.variant(opcode := 0x05, tag := 1, arity := 0)
    True
};
/// The unit type, representing absence of data.
@musi.lang(name := "Unit")
export let Unit := data { ; };
/// The uninhabited type, used for non-terminating functions.
@musi.lang(name := "Empty")
export let Empty := data { | };

// --- Platform-sized numeric types ---
/// Platform-sized signed integer.
@musi.lang(name := "Int")
export opaque let Int := data { ; };
/// Platform-sized unsigned integer.
@musi.lang(name := "Nat")
export opaque let Nat := data { ; };
/// Platform-sized floating-point number.
@musi.lang(name := "Float")
export opaque let Float := data { ; };

// --- Sized integers ---
/// 8-bit signed integer.
@musi.lang(name := "Int8")
export opaque let Int8 := data { ; };
/// 16-bit signed integer.
@musi.lang(name := "Int16")
export opaque let Int16 := data { ; };
/// 32-bit signed integer.
@musi.lang(name := "Int32")
export opaque let Int32 := data { ; };
/// 64-bit signed integer.
@musi.lang(name := "Int64")
export opaque let Int64 := data { ; };

// --- Sized unsigned integers ---
/// 8-bit unsigned integer.
@musi.lang(name := "Nat8")
export opaque let Nat8 := data { ; };
/// 16-bit unsigned integer.
@musi.lang(name := "Nat16")
export opaque let Nat16 := data { ; };
/// 32-bit unsigned integer.
@musi.lang(name := "Nat32")
export opaque let Nat32 := data { ; };
/// 64-bit unsigned integer.
@musi.lang(name := "Nat64")
export opaque let Nat64 := data { ; };

// --- Sized floats ---
/// 32-bit floating-point number.
@musi.lang(name := "Float32")
export opaque let Float32 := data { ; };
/// 64-bit floating-point number.
@musi.lang(name := "Float64")
export opaque let Float64 := data { ; };

// --- Text types ---
/// A single Unicode character.
@musi.lang(name := "Rune")
export opaque let Rune := data { ; };
/// A sequence of byte values representing UTF-8 encoded text.
@musi.lang(name := "String")
export opaque let String := data { ; };
/// A NUL-terminated C string used for FFI boundaries.
@musi.lang(name := "CString")
export opaque let CString := data { ; };
/// An opaque C pointer used for FFI boundaries.
@musi.lang(name := "CPtr")
export opaque let CPtr := data { ; };

// --- Collection types ---
/// A fixed-size array of elements of type `T`.
@musi.lang(name := "Array")
export opaque let Array [T] := data { ; };
/// A linked list of elements of type `T`.
@musi.lang(name := "List")
export opaque let List [T] := data { ; };

// --- Universe types - compiler intrinsics, declared here for visibility. ---
/// The type of types.
@musi.lang(name := "Type")
export opaque let Type := data { ; };
/// A type that can represent any value (escape hatch for the type system).
@musi.lang(name := "Any")
export opaque let Any := data { ; };
/// A placeholder for unknown or yet-to-be-determined types.
@musi.lang(name := "Unknown")
export opaque let Unknown := data { ; };

// --- Core type classes ---
/// Equality type class for types that can be compared for equality.
export let Eq [T] := class {
    /// Equality operator: returns `True` if values are equal.
    @musi.intrinsic(opcode := 0x25)  // CmpEq
    let (_ = _) (a : T, b : T) : Bool;
    /// Inequality operator: returns `True` if values are not equal.
    let (_ /= _) (a : T, b : T) : Bool;
};

/// Ordering type class for types that have a total order.
export let Ord [T] := class where T : Eq {
    /// Less-than operator.
    @musi.intrinsic(opcode := 0x27)  // CmpLt
    let (_ < _) (a : T, b : T) : Bool;
    /// Greater-than operator.
    @musi.intrinsic(opcode := 0x28)  // CmpGt
    let (_ > _) (a : T, b : T) : Bool;
    /// Less-than-or-equal operator.
    @musi.intrinsic(opcode := 0x29)  // CmpLeq
    let (_ <= _) (a : T, b : T) : Bool;
    /// Greater-than-or-equal operator.
    @musi.intrinsic(opcode := 0x2A)  // CmpGeq
    let (_ >= _) (a : T, b : T) : Bool;
};

/// Numeric type class for types that support arithmetic operations.
export let Num [T] := class where T : Eq {
    /// Addition operator.
    @musi.intrinsic(opcode := 0x14)  // IAdd
    let (_ + _) (a : T, b : T) : T;
    /// Subtraction operator.
    @musi.intrinsic(opcode := 0x15)  // ISub
    let (_ - _) (a : T, b : T) : T;
    /// Multiplication operator.
    @musi.intrinsic(opcode := 0x16)  // IMul
    let (_ * _) (a : T, b : T) : T;
    /// Division operator.
    @musi.intrinsic(opcode := 0x17)  // IDiv
    let (_ / _) (a : T, b : T) : T;
    /// Remainder operator.
    @musi.intrinsic(opcode := 0x18)  // IRem
    let (_ % _) (a : T, b : T) : T;
    /// Negation operator.
    @musi.intrinsic(opcode := 0x19)  // INeg
    let (- _) (a : T) : T;
};

/// Type class for types that can be converted to a string representation.
export let Show [T] := class {
    /// Converts a value to its string representation.
    let show (a : T) : String;
};

/// Bitwise shift operations for integer types.
export let Bits [T] := class {
    @musi.intrinsic(opcode := 0x23)  // Shl
    let (_ shl _) (a : T, n : Int) : T;
    @musi.intrinsic(opcode := 0x24)  // Shr
    let (_ shr _) (a : T, n : Int) : T;
};
