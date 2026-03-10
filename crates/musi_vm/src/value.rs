//! NaN-boxed 64-bit value representation.
//!
//! Tag occupies the top 16 bits (bits 63..=48). Tags in the range
//! `0x7FF1..=0x7FF9` are reserved for non-float MUSI values; everything
//! outside that range is an IEEE 754 double-precision float.

use core::fmt;

use crate::error::VmError;

// Non-float tag sentinels (top 16 bits).
const TAG_INT: u16 = 0x7FF1; // signed 48-bit integer (sign-extended)
const TAG_UINT: u16 = 0x7FF2; // unsigned 48-bit integer
const TAG_BOOL: u16 = 0x7FF3;
const TAG_RUNE: u16 = 0x7FF4; // Unicode scalar value (char)
const TAG_REF: u16 = 0x7FF5; // GC heap index (48-bit)
const TAG_PTR: u16 = 0x7FF6; // raw pointer — unsafe
const TAG_FN: u16 = 0x7FF7; // fn_id (32-bit)
const TAG_TASK: u16 = 0x7FF8; // task handle (32-bit)
const TAG_CHAN: u16 = 0x7FF9; // channel handle (32-bit)
const TAG_UNIT: u16 = 0x7FFA;

/// Lower bound of the MUSI non-float tag range.
const MUSI_TAG_LO: u16 = 0x7FF1;
/// Upper bound of the MUSI non-float tag range.
const MUSI_TAG_HI: u16 = 0x7FFA;

/// 48-bit mask for the payload portion of a `Value`.
const PAYLOAD_MASK: u64 = 0x0000_FFFF_FFFF_FFFF;

/// u16 → u64 widening (const-safe, no `as`).
const fn u16_as_u64(v: u16) -> u64 {
    let bytes = v.to_le_bytes();
    u64::from_le_bytes([bytes[0], bytes[1], 0, 0, 0, 0, 0, 0])
}

/// i64 → u64 bit reinterpretation (const-safe, no `as`).
const fn i64_to_bits(n: i64) -> u64 {
    u64::from_le_bytes(n.to_le_bytes())
}

/// u32 → u64 widening (const-safe, no `as`).
const fn u32_as_u64(v: u32) -> u64 {
    let bytes = v.to_le_bytes();
    u64::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3], 0, 0, 0, 0])
}

/// char → u64 (const-safe). char → u32 requires `as`; isolated here.
#[allow(clippy::as_conversions)]
const fn char_as_u64(c: char) -> u64 {
    c as u64
}

/// A NaN-boxed 64-bit runtime value.
///
/// Floats are stored as raw `f64` bits. All other types occupy the NaN space
/// (`0x7FF1`–`0x7FFA` in the top 16 bits).
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Value(pub u64);

impl Value {
    /// The unit value.
    pub const UNIT: Self = Self(u16_as_u64(TAG_UNIT) << 48);

    /// True boolean.
    pub const TRUE: Self = Self::from_bool(true);
    /// False boolean.
    pub const FALSE: Self = Self::from_bool(false);

    /// Wrap a signed integer (only the low 48 bits are stored).
    #[must_use]
    pub const fn from_int(n: i64) -> Self {
        let payload = i64_to_bits(n) & PAYLOAD_MASK;
        Self((u16_as_u64(TAG_INT) << 48) | payload)
    }

    /// Wrap an unsigned integer (only the low 48 bits are stored).
    #[must_use]
    pub const fn from_uint(n: u64) -> Self {
        let payload = n & PAYLOAD_MASK;
        Self((u16_as_u64(TAG_UINT) << 48) | payload)
    }

    /// Wrap an `f64`.
    #[must_use]
    pub const fn from_float(f: f64) -> Self {
        Self(f.to_bits())
    }

    /// Wrap a boolean.
    #[must_use]
    pub const fn from_bool(b: bool) -> Self {
        let payload = if b { 1u64 } else { 0u64 };
        Self((u16_as_u64(TAG_BOOL) << 48) | payload)
    }

    /// Wrap a Unicode scalar value.
    #[must_use]
    pub const fn from_rune(c: char) -> Self {
        let payload = char_as_u64(c) & PAYLOAD_MASK;
        Self((u16_as_u64(TAG_RUNE) << 48) | payload)
    }

    /// Wrap a function id.
    #[must_use]
    pub const fn from_fn_id(id: u32) -> Self {
        let payload = u32_as_u64(id);
        Self((u16_as_u64(TAG_FN) << 48) | payload)
    }

    /// Wrap a heap index (48-bit address).
    #[must_use]
    pub const fn from_ref(ptr: u64) -> Self {
        let payload = ptr & PAYLOAD_MASK;
        Self((u16_as_u64(TAG_REF) << 48) | payload)
    }

    /// Wrap a task id (32-bit).
    #[must_use]
    pub const fn from_task(id: u32) -> Self {
        let payload = u32_as_u64(id);
        Self((u16_as_u64(TAG_TASK) << 48) | payload)
    }

    /// Wrap a channel id (32-bit).
    #[must_use]
    pub const fn from_chan(id: u32) -> Self {
        let payload = u32_as_u64(id);
        Self((u16_as_u64(TAG_CHAN) << 48) | payload)
    }

    /// Extract the top 16 bits as a tag.
    #[must_use]
    pub const fn tag(self) -> u16 {
        let shifted = self.0 >> 48;
        let bytes = shifted.to_le_bytes();
        u16::from_le_bytes([bytes[0], bytes[1]])
    }

    /// Return `true` if this value is an IEEE 754 float (not a tagged MUSI value).
    #[must_use]
    pub const fn is_float(self) -> bool {
        let t = self.tag();
        t < MUSI_TAG_LO || t > MUSI_TAG_HI
    }

    /// Return `true` if this value is the unit value.
    #[must_use]
    pub const fn is_unit(self) -> bool {
        self.tag() == TAG_UNIT
    }

    /// Sign-extend the 48-bit payload to a full `i64`.
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if this value is not a tagged int.
    pub const fn as_int(self) -> Result<i64, VmError> {
        if self.tag() != TAG_INT {
            return Err(VmError::TypeError {
                expected: "int",
                found: tag_name(self.tag()),
            });
        }
        let raw = self.0 & PAYLOAD_MASK;
        let sign_bit: u64 = 1 << 47;
        let extended = if raw & sign_bit != 0 {
            raw | !PAYLOAD_MASK
        } else {
            raw
        };
        Ok(i64::from_le_bytes(extended.to_le_bytes()))
    }

    /// Extract an unsigned integer payload.
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if this value is not a tagged uint.
    pub const fn as_uint(self) -> Result<u64, VmError> {
        if self.tag() != TAG_UINT {
            return Err(VmError::TypeError {
                expected: "uint",
                found: tag_name(self.tag()),
            });
        }
        Ok(self.0 & PAYLOAD_MASK)
    }

    /// Extract a float value.
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if this value is not a float.
    pub const fn as_float(self) -> Result<f64, VmError> {
        if !self.is_float() {
            return Err(VmError::TypeError {
                expected: "float",
                found: tag_name(self.tag()),
            });
        }
        Ok(f64::from_bits(self.0))
    }

    /// Extract a boolean value.
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if this value is not a tagged bool.
    pub const fn as_bool(self) -> Result<bool, VmError> {
        if self.tag() != TAG_BOOL {
            return Err(VmError::TypeError {
                expected: "bool",
                found: tag_name(self.tag()),
            });
        }
        Ok(self.0 & 1 != 0)
    }

    /// Extract the heap index (usize).
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if not a ref, or `Malformed` if the index overflows.
    pub fn as_ref(self) -> Result<usize, VmError> {
        if self.tag() != TAG_REF {
            return Err(VmError::TypeError {
                expected: "ref",
                found: tag_name(self.tag()),
            });
        }
        let payload = self.0 & PAYLOAD_MASK;
        usize::try_from(payload).map_err(|_| VmError::Malformed {
            desc: "heap index overflows usize".into(),
        })
    }

    /// Try to extract a heap reference index without producing an error.
    ///
    /// Returns `None` if this value is not a ref or the index overflows `usize`.
    #[must_use]
    pub fn try_as_ref(self) -> Option<usize> {
        if self.tag() != TAG_REF {
            return None;
        }
        let payload = self.0 & PAYLOAD_MASK;
        usize::try_from(payload).ok()
    }

    /// Extract a task id.
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if not a task handle.
    pub fn as_task_id(self) -> Result<u32, VmError> {
        if self.tag() != TAG_TASK {
            return Err(VmError::TypeError {
                expected: "task",
                found: tag_name(self.tag()),
            });
        }
        let payload = self.0 & 0xFFFF_FFFF;
        u32::try_from(payload).map_err(|_| VmError::Malformed {
            desc: "task_id overflow".into(),
        })
    }

    /// Extract a channel id.
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if not a channel handle.
    pub fn as_chan_id(self) -> Result<u32, VmError> {
        if self.tag() != TAG_CHAN {
            return Err(VmError::TypeError {
                expected: "chan",
                found: tag_name(self.tag()),
            });
        }
        let payload = self.0 & 0xFFFF_FFFF;
        u32::try_from(payload).map_err(|_| VmError::Malformed {
            desc: "chan_id overflow".into(),
        })
    }

    /// Extract the function id.
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if not a fn, or `Malformed` if the id overflows.
    pub fn as_fn_id(self) -> Result<u32, VmError> {
        if self.tag() != TAG_FN {
            return Err(VmError::TypeError {
                expected: "fn",
                found: tag_name(self.tag()),
            });
        }
        let payload = self.0 & 0xFFFF_FFFF;
        u32::try_from(payload).map_err(|_| VmError::Malformed {
            desc: "fn_id overflow".into(),
        })
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_unit() {
            write!(f, "unit")
        } else if self.is_float() {
            write!(f, "f64({})", f64::from_bits(self.0))
        } else {
            match self.tag() {
                TAG_INT => write!(f, "int({})", self.as_int().unwrap_or(0)),
                TAG_UINT => write!(f, "uint({})", self.0 & PAYLOAD_MASK),
                TAG_BOOL => write!(f, "bool({})", self.0 & 1 != 0),
                TAG_RUNE => write!(
                    f,
                    "rune({})",
                    char::from_u32(u32::try_from(self.0 & PAYLOAD_MASK).unwrap_or(0))
                        .unwrap_or('\0')
                ),
                TAG_REF => write!(f, "ref({})", self.0 & PAYLOAD_MASK),
                TAG_FN => write!(f, "fn({})", self.0 & 0xFFFF_FFFF),
                TAG_TASK => write!(f, "task({})", self.0 & 0xFFFF_FFFF),
                TAG_CHAN => write!(f, "chan({})", self.0 & 0xFFFF_FFFF),
                TAG_PTR => write!(f, "ptr({})", self.0 & PAYLOAD_MASK),
                t => write!(
                    f,
                    "value(tag={t:#06x}, payload={:#014x})",
                    self.0 & PAYLOAD_MASK
                ),
            }
        }
    }
}

const fn tag_name(tag: u16) -> &'static str {
    match tag {
        TAG_INT => "int",
        TAG_UINT => "uint",
        TAG_BOOL => "bool",
        TAG_RUNE => "rune",
        TAG_REF => "ref",
        TAG_PTR => "ptr",
        TAG_FN => "fn",
        TAG_TASK => "task",
        TAG_CHAN => "chan",
        TAG_UNIT => "unit",
        _ => "float",
    }
}
