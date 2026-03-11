/// C ABI type aliases for foreign function interface.
///
/// These map Musi types to their C equivalents for use with `foreign` declarations.

/// Signed 32-bit integer.
///
/// Corresponds to `int` in C.
export let CInt := Int32;

/// Unsigned 32-bit integer.
///
/// Corresponds to `unsigned int` in C.
export let CUInt := UInt32;

/// Signed 8-bit integer.
///
/// Corresponds to `char` in C.
export let CChar := Int8;

/// Unsigned 64-bit integer.
///
/// Corresponds to `size_t` in C.
export let CSize := UInt64;

/// 64-bit floating-point number.
///
/// Corresponds to `double` in C.
export let CDouble := Float64;

/// 32-bit floating-point number.
///
/// Corresponds to `float` in C.
export let CFloat := Float32;
