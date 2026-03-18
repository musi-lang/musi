use core::fmt;

use crate::error::VmError;
use crate::heap::{Heap, HeapPayload};

// IEEE 754 quiet NaN has exponent=0x7FF and bit 51 set. We require
// bits 62-50 all set (QNAN mask), giving 8 tag slots via bits 63, 49, 48.
// Floats whose bits collide with tag space are canonicalized.

const QNAN: u64 = 0x7FFC_0000_0000_0000;
const CANONICAL_NAN: u64 = 0x7FF8_0000_0000_0000;
const PAYLOAD_MASK: u64 = 0x0000_FFFF_FFFF_FFFF;
const TAG_MASK: u64 = 0xFFFF_0000_0000_0000;

const TAG_INT: u64 = 0x7FFC_0000_0000_0000;
const TAG_NAT: u64 = 0x7FFD_0000_0000_0000;
const TAG_BOOL: u64 = 0x7FFE_0000_0000_0000;
const TAG_UNIT: u64 = 0x7FFF_0000_0000_0000;
const TAG_REF: u64 = 0xFFFC_0000_0000_0000;
const TAG_FN: u64 = 0xFFFD_0000_0000_0000;
const TAG_RUNE: u64 = 0xFFFE_0000_0000_0000;
const TAG_TACH: u64 = 0xFFFF_0000_0000_0000; // task / chan combined

const CHAN_BIT: u64 = 1u64 << 47;

const INT_48_MIN: i64 = -(1i64 << 47);
const INT_48_MAX: i64 = (1i64 << 47) - 1;

// Lossless widening helpers for const context where `u64::from()` is unavailable.
#[allow(clippy::as_conversions)]
const fn widen(n: u32) -> u64 {
    n as u64
}

#[allow(clippy::as_conversions)]
const fn bool_to_u64(b: bool) -> u64 {
    b as u64
}

#[allow(clippy::as_conversions)]
const fn char_to_u64(c: char) -> u64 {
    c as u64
}

/// NaN-boxed runtime value (8 bytes, cache-friendly).
///
/// - Floats: stored as raw IEEE 754 bits (NaN canonicalized)
/// - Tagged values: top 16 bits = type tag, bottom 48 bits = payload
/// - Wide integers (>48-bit): heap-boxed behind a `Ref` with a `BoxedInt`/`BoxedNat` payload
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Value(pub u64);

impl Value {
    pub const UNIT: Self = Self(TAG_UNIT);
    pub const TRUE: Self = Self(TAG_BOOL | 1);
    pub const FALSE: Self = Self(TAG_BOOL);
    pub const NAN: Self = Self(CANONICAL_NAN);

    /// Create an int. Truncates to 48 bits (wrapping).
    /// Use `from_int_wide` for values outside [-2^47, 2^47-1].
    #[must_use]
    pub const fn from_int(n: i64) -> Self {
        Self(TAG_INT | (n.cast_unsigned() & PAYLOAD_MASK))
    }

    /// Create a nat. Truncates to 48 bits.
    /// Use `from_nat_wide` for values > 2^48-1.
    #[must_use]
    pub const fn from_nat(n: u64) -> Self {
        Self(TAG_NAT | (n & PAYLOAD_MASK))
    }

    #[must_use]
    pub const fn from_float(f: f64) -> Self {
        let bits = f.to_bits();
        if (bits & QNAN) == QNAN {
            Self(CANONICAL_NAN)
        } else {
            Self(bits)
        }
    }

    #[must_use]
    pub const fn from_bool(b: bool) -> Self {
        Self(TAG_BOOL | bool_to_u64(b))
    }

    #[must_use]
    pub const fn from_rune(c: char) -> Self {
        Self(TAG_RUNE | char_to_u64(c))
    }

    #[must_use]
    pub const fn from_fn_id(id: u32) -> Self {
        Self(TAG_FN | widen(id))
    }

    #[must_use]
    pub const fn from_ref(ptr: u64) -> Self {
        Self(TAG_REF | (ptr & PAYLOAD_MASK))
    }

    #[must_use]
    pub const fn from_task(id: u32) -> Self {
        Self(TAG_TACH | widen(id))
    }

    #[must_use]
    pub const fn from_chan(id: u32) -> Self {
        Self(TAG_TACH | CHAN_BIT | widen(id))
    }

    #[must_use]
    pub fn from_int_wide(n: i64, heap: &mut Heap) -> Self {
        if (INT_48_MIN..=INT_48_MAX).contains(&n) {
            Self::from_int(n)
        } else {
            let ptr = heap.alloc_wide_int(n);
            Self::from_ref(ptr)
        }
    }

    #[must_use]
    pub fn from_nat_wide(n: u64, heap: &mut Heap) -> Self {
        if n <= PAYLOAD_MASK {
            Self::from_nat(n)
        } else {
            let ptr = heap.alloc_wide_nat(n);
            Self::from_ref(ptr)
        }
    }

    #[must_use]
    pub const fn is_float(self) -> bool {
        (self.0 & QNAN) != QNAN
    }

    #[must_use]
    pub const fn is_unit(self) -> bool {
        self.0 == TAG_UNIT
    }

    #[must_use]
    pub const fn is_int(self) -> bool {
        (self.0 & TAG_MASK) == TAG_INT
    }

    #[must_use]
    pub const fn is_nat(self) -> bool {
        (self.0 & TAG_MASK) == TAG_NAT
    }

    #[must_use]
    pub const fn is_bool(self) -> bool {
        (self.0 & TAG_MASK) == TAG_BOOL
    }

    #[must_use]
    pub const fn is_rune(self) -> bool {
        (self.0 & TAG_MASK) == TAG_RUNE
    }

    #[must_use]
    pub const fn is_ref(self) -> bool {
        (self.0 & TAG_MASK) == TAG_REF
    }

    #[must_use]
    pub const fn is_fn(self) -> bool {
        (self.0 & TAG_MASK) == TAG_FN
    }

    /// Top 16 bits - type discriminator for NaN-boxed dispatch.
    #[must_use]
    #[allow(clippy::cast_possible_truncation, clippy::as_conversions)] // u64 >> 48 always fits u16
    pub const fn tag(self) -> u16 {
        (self.0 >> 48) as u16
    }

    /// Extracts the lower 32 bits of the NaN-boxed payload.
    ///
    /// # Panics
    ///
    /// Panics in debug mode if the payload exceeds `u32::MAX`.
    #[allow(clippy::cast_possible_truncation, clippy::as_conversions)]
    const fn payload_u32(self) -> u32 {
        let p = self.0 & PAYLOAD_MASK;
        debug_assert!(p <= 0xFFFF_FFFF, "payload exceeds u32");
        p as u32
    }

    /// Extracts the lower 32 bits of the payload after applying a custom mask.
    ///
    /// # Panics
    ///
    /// Panics in debug mode if the masked payload exceeds `u32::MAX`.
    #[allow(clippy::cast_possible_truncation, clippy::as_conversions)]
    const fn payload_u32_masked(self, mask: u64) -> u32 {
        let p = self.0 & mask;
        debug_assert!(p <= 0xFFFF_FFFF, "payload exceeds u32");
        p as u32
    }

    /// Sign-extends a 48-bit payload to i64 via arithmetic right-shift.
    #[allow(clippy::cast_possible_wrap, clippy::as_conversions)]
    const fn sign_extend_48(raw: u64) -> i64 {
        ((raw << 16) as i64) >> 16
    }

    /// Extract 48-bit sign-extended int.
    ///
    /// # Errors
    ///
    /// Returns [`VmError::TypeError`] if the value is not an int.
    pub const fn as_int(self) -> Result<i64, VmError> {
        if (self.0 & TAG_MASK) == TAG_INT {
            let raw = self.0 & PAYLOAD_MASK;
            Ok(Self::sign_extend_48(raw))
        } else {
            Err(VmError::TypeError {
                expected: "int",
                found: self.type_name(),
            })
        }
    }

    /// # Errors
    ///
    /// Returns [`VmError::TypeError`] if the value is not a nat.
    pub const fn as_nat(self) -> Result<u64, VmError> {
        if (self.0 & TAG_MASK) == TAG_NAT {
            Ok(self.0 & PAYLOAD_MASK)
        } else {
            Err(VmError::TypeError {
                expected: "nat",
                found: self.type_name(),
            })
        }
    }

    /// # Errors
    ///
    /// Returns [`VmError::TypeError`] if the value is not a float.
    pub const fn as_float(self) -> Result<f64, VmError> {
        if self.is_float() {
            Ok(f64::from_bits(self.0))
        } else {
            Err(VmError::TypeError {
                expected: "float",
                found: self.type_name(),
            })
        }
    }

    /// # Errors
    ///
    /// Returns [`VmError::TypeError`] if the value is not a bool.
    pub const fn as_bool(self) -> Result<bool, VmError> {
        if (self.0 & TAG_MASK) == TAG_BOOL {
            Ok((self.0 & 1) != 0)
        } else {
            Err(VmError::TypeError {
                expected: "bool",
                found: self.type_name(),
            })
        }
    }

    /// Interprets the value as a boolean condition, accepting both bools and
    /// ints (non-zero = true). Used by conditional jumps.
    ///
    /// # Errors
    ///
    /// Returns [`VmError::TypeError`] if the value is neither a bool nor an int.
    pub fn as_truthy(self) -> Result<bool, VmError> {
        if (self.0 & TAG_MASK) == TAG_BOOL {
            Ok((self.0 & 1) != 0)
        } else if let Ok(n) = self.as_int() {
            Ok(n != 0)
        } else {
            Err(VmError::TypeError {
                expected: "bool",
                found: self.type_name(),
            })
        }
    }

    /// # Errors
    ///
    /// Returns [`VmError::TypeError`] if the value is not a rune, or
    /// [`VmError::Malformed`] if the codepoint is invalid.
    pub fn as_rune(self) -> Result<char, VmError> {
        if (self.0 & TAG_MASK) == TAG_RUNE {
            let code = self.payload_u32();
            char::from_u32(code).ok_or_else(|| VmError::Malformed {
                desc: "invalid rune codepoint".into(),
            })
        } else {
            Err(VmError::TypeError {
                expected: "rune",
                found: self.type_name(),
            })
        }
    }

    /// # Errors
    ///
    /// Returns [`VmError::TypeError`] if the value is not a ref, or
    /// [`VmError::Malformed`] if the heap index overflows usize.
    pub fn as_ref(self) -> Result<usize, VmError> {
        if (self.0 & TAG_MASK) == TAG_REF {
            let idx = self.payload_u32();
            usize::try_from(idx).map_err(|_| VmError::Malformed {
                desc: "heap index overflows usize".into(),
            })
        } else {
            Err(VmError::TypeError {
                expected: "ref",
                found: self.type_name(),
            })
        }
    }

    #[must_use]
    pub fn try_as_ref(self) -> Option<usize> {
        if (self.0 & TAG_MASK) == TAG_REF {
            usize::try_from(self.payload_u32()).ok()
        } else {
            None
        }
    }

    /// # Errors
    ///
    /// Returns [`VmError::TypeError`] if the value is not a function.
    pub const fn as_fn_id(self) -> Result<u32, VmError> {
        if (self.0 & TAG_MASK) == TAG_FN {
            Ok(self.payload_u32())
        } else {
            Err(VmError::TypeError {
                expected: "fn",
                found: self.type_name(),
            })
        }
    }

    /// # Errors
    ///
    /// Returns [`VmError::TypeError`] if the value is not a task.
    pub const fn as_task_id(self) -> Result<u32, VmError> {
        if (self.0 & TAG_MASK) == TAG_TACH && (self.0 & CHAN_BIT) == 0 {
            Ok(self.payload_u32())
        } else {
            Err(VmError::TypeError {
                expected: "task",
                found: self.type_name(),
            })
        }
    }

    /// # Errors
    ///
    /// Returns [`VmError::TypeError`] if the value is not a channel.
    pub const fn as_chan_id(self) -> Result<u32, VmError> {
        if (self.0 & TAG_MASK) == TAG_TACH && (self.0 & CHAN_BIT) != 0 {
            Ok(self.payload_u32_masked(PAYLOAD_MASK & !CHAN_BIT))
        } else {
            Err(VmError::TypeError {
                expected: "chan",
                found: self.type_name(),
            })
        }
    }

    /// Extract int: inline 48-bit or heap-boxed wide int.
    ///
    /// # Errors
    ///
    /// Returns [`VmError::TypeError`] if the value is not an int.
    pub fn as_int_wide(self, heap: &Heap) -> Result<i64, VmError> {
        if let Ok(n) = self.as_int() {
            return Ok(n);
        }
        if let Ok(ptr) = self.as_ref() {
            if let Ok(obj) = heap.get(ptr) {
                if let HeapPayload::BoxedInt(n) = obj.payload {
                    return Ok(n);
                }
            }
        }
        Err(VmError::TypeError {
            expected: "int",
            found: self.type_name(),
        })
    }

    /// Extract nat: inline 48-bit or heap-boxed wide nat.
    ///
    /// # Errors
    ///
    /// Returns [`VmError::TypeError`] if the value is not a nat.
    pub fn as_nat_wide(self, heap: &Heap) -> Result<u64, VmError> {
        if let Ok(n) = self.as_nat() {
            return Ok(n);
        }
        if let Ok(ptr) = self.as_ref() {
            if let Ok(obj) = heap.get(ptr) {
                if let HeapPayload::BoxedNat(n) = obj.payload {
                    return Ok(n.cast_unsigned());
                }
            }
        }
        Err(VmError::TypeError {
            expected: "nat",
            found: self.type_name(),
        })
    }

    #[must_use]
    pub const fn type_name(self) -> &'static str {
        if self.is_float() {
            return "float";
        }
        match self.0 & TAG_MASK {
            TAG_INT => "int",
            TAG_NAT => "nat",
            TAG_BOOL => "bool",
            TAG_UNIT => "unit",
            TAG_REF => "ref",
            TAG_FN => "fn",
            TAG_RUNE => "rune",
            TAG_TACH => {
                if (self.0 & CHAN_BIT) != 0 {
                    "chan"
                } else {
                    "task"
                }
            }
            _ => "unknown",
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_float() {
            return write!(f, "f64({})", f64::from_bits(self.0));
        }
        match self.0 & TAG_MASK {
            TAG_INT => {
                let raw = self.0 & PAYLOAD_MASK;
                let n = Self::sign_extend_48(raw);
                write!(f, "int({n})")
            }
            TAG_NAT => write!(f, "nat({})", self.0 & PAYLOAD_MASK),
            TAG_BOOL => write!(f, "bool({})", (self.0 & 1) != 0),
            TAG_UNIT => write!(f, "unit"),
            TAG_REF => write!(f, "ref({})", self.payload_u32()),
            TAG_FN => write!(f, "fn({})", self.payload_u32()),
            TAG_RUNE => {
                let code = self.payload_u32();
                match char::from_u32(code) {
                    Some(c) => write!(f, "rune({c})"),
                    None => write!(f, "rune(<invalid:{code}>)"),
                }
            }
            TAG_TACH => {
                if (self.0 & CHAN_BIT) != 0 {
                    write!(
                        f,
                        "chan({})",
                        self.payload_u32_masked(PAYLOAD_MASK & !CHAN_BIT)
                    )
                } else {
                    write!(f, "task({})", self.payload_u32())
                }
            }
            _ => write!(f, "unknown({:#018x})", self.0),
        }
    }
}

/// Check if two values are equal when their raw bits differ.
///
/// Handles heap-boxed wide integers and heap-allocated strings.
pub fn wide_values_equal(a: Value, b: Value, heap: &Heap) -> bool {
    if let (Ok(ai), Ok(bi)) = (a.as_int_wide(heap), b.as_int_wide(heap)) {
        return ai == bi;
    }
    if let (Ok(an), Ok(bn)) = (a.as_nat_wide(heap), b.as_nat_wide(heap)) {
        return an == bn;
    }
    if let (Ok(ai), Ok(bi)) = (a.as_ref(), b.as_ref())
        && let (Ok(oa), Ok(ob)) = (heap.get(ai), heap.get(bi))
    {
        match (&oa.payload, &ob.payload) {
            (HeapPayload::Str { data: da, .. }, HeapPayload::Str { data: db, .. }) => {
                return da == db;
            }
            (HeapPayload::Array { elems: ea, .. }, HeapPayload::Array { elems: eb, .. }) => {
                if ea.len() != eb.len() {
                    return false;
                }
                return ea
                    .iter()
                    .zip(eb.iter())
                    .all(|(&va, &vb)| va.0 == vb.0 || wide_values_equal(va, vb, heap));
            }
            (
                HeapPayload::Record {
                    tag: ta,
                    fields: fa,
                    ..
                },
                HeapPayload::Record {
                    tag: tb,
                    fields: fb,
                    ..
                },
            ) => {
                if ta != tb || fa.len() != fb.len() {
                    return false;
                }
                return fa
                    .iter()
                    .zip(fb.iter())
                    .all(|(&va, &vb)| va.0 == vb.0 || wide_values_equal(va, vb, heap));
            }
            _ => {}
        }
    }
    false
}
