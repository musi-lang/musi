#![allow(
    clippy::as_conversions,
    clippy::arithmetic_side_effects,
    clippy::cast_lossless,
    clippy::cast_possible_wrap,
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss
)]

use std::fmt;

use music_il::format::{NAN_BOX_BOOL, NAN_BOX_PTR, NAN_BOX_SMI, NAN_BOX_TAG, NAN_BOX_UNIT};

const QNAN: u64 = 0x7FF8_0000_0000_0000;
const PAYLOAD_MASK: u64 = 0x0000_FFFF_FFFF_FFFF;
const TAG_SHIFT: u32 = 48;
/// Sentinel for canonical NaN -- uses tag slot 0b111 (unused by any value kind).
const NAN_SENTINEL: u64 = QNAN | (0b111 << TAG_SHIFT);

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Value(u64);

impl Value {
    pub const UNIT: Self = Self::tagged(NAN_BOX_UNIT as u64, 0);
    pub const TRUE: Self = Self::tagged(NAN_BOX_BOOL as u64, 1);
    pub const FALSE: Self = Self::tagged(NAN_BOX_BOOL as u64, 0);
    pub const ZERO: Self = Self::tagged(NAN_BOX_SMI as u64, 0);
    pub const ONE: Self = Self::tagged(NAN_BOX_SMI as u64, 1);

    const fn tagged(tag: u64, payload: u64) -> Self {
        Self(QNAN | (tag << TAG_SHIFT) | (payload & PAYLOAD_MASK))
    }

    #[must_use]
    pub const fn from_int(i: i64) -> Self {
        debug_assert!(
            i >= -(1i64 << 47) && i < (1i64 << 47),
            "from_int: value outside 48-bit signed range"
        );
        Self(QNAN | ((NAN_BOX_SMI as u64) << TAG_SHIFT) | (i as u64 & PAYLOAD_MASK))
    }

    #[must_use]
    pub const fn from_float(f: f64) -> Self {
        let bits = f.to_bits();
        if bits & QNAN == QNAN {
            Self(NAN_SENTINEL)
        } else {
            Self(bits)
        }
    }

    #[must_use]
    pub const fn from_ptr(idx: usize) -> Self {
        debug_assert!(idx < (1usize << 48), "from_ptr: index exceeds 48-bit range");
        Self::tagged(NAN_BOX_PTR as u64, idx as u64)
    }

    #[must_use]
    pub const fn from_tag(idx: u16) -> Self {
        Self::tagged(NAN_BOX_TAG as u64, idx as u64)
    }

    #[must_use]
    pub const fn is_tag(self) -> bool {
        !self.is_float() && self.tag() == NAN_BOX_TAG
    }

    #[must_use]
    pub const fn as_tag_idx(self) -> u16 {
        debug_assert!(
            (self.0 & PAYLOAD_MASK) <= u16::MAX as u64,
            "as_tag_idx: payload exceeds u16 range"
        );
        (self.0 & PAYLOAD_MASK) as u16
    }

    #[must_use]
    pub const fn from_bool(b: bool) -> Self {
        if b {
            Self::TRUE
        } else {
            Self::FALSE
        }
    }

    #[must_use]
    pub const fn is_ptr(self) -> bool {
        !self.is_float() && self.tag() == NAN_BOX_PTR
    }

    #[must_use]
    pub const fn as_ptr_idx(self) -> usize {
        (self.0 & PAYLOAD_MASK) as usize
    }

    #[must_use]
    pub const fn is_float(self) -> bool {
        (self.0 & QNAN) != QNAN || self.0 == NAN_SENTINEL
    }

    #[must_use]
    pub const fn is_int(self) -> bool {
        self.tag() == NAN_BOX_SMI
    }

    #[must_use]
    pub const fn is_bool(self) -> bool {
        self.tag() == NAN_BOX_BOOL
    }

    #[must_use]
    pub const fn is_unit(self) -> bool {
        self.tag() == NAN_BOX_UNIT
    }

    #[must_use]
    pub const fn as_int(self) -> i64 {
        let payload = self.0 & PAYLOAD_MASK;
        // Sign-extend from bit 47
        (payload as i64) << 16 >> 16
    }

    #[must_use]
    pub const fn as_float(self) -> f64 {
        f64::from_bits(self.0)
    }

    #[must_use]
    pub const fn as_bool(self) -> bool {
        (self.0 & PAYLOAD_MASK) != 0
    }

    /// The 3-bit NaN-box tag, or 0 for floats (no tag bits set).
    #[must_use]
    pub(crate) const fn nan_tag(self) -> u8 {
        if self.is_float() {
            return 0;
        }
        ((self.0 >> TAG_SHIFT) & 0x7) as u8
    }

    const fn tag(self) -> u8 {
        if self.is_float() {
            return u8::MAX;
        }
        ((self.0 >> TAG_SHIFT) & 0x7) as u8
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_float() {
            write!(f, "Float({})", self.as_float())
        } else if self.is_int() {
            write!(f, "Int({})", self.as_int())
        } else if self.is_bool() {
            write!(f, "Bool({})", self.as_bool())
        } else if self.is_unit() {
            write!(f, "Unit")
        } else if self.is_ptr() {
            write!(f, "Ptr({})", self.as_ptr_idx())
        } else if self.is_tag() {
            write!(f, "Tag({})", self.as_tag_idx())
        } else {
            write!(f, "Value(0x{:016x})", self.0)
        }
    }
}

#[cfg(test)]
mod tests;
