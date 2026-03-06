/// Every built-in function callable from Musi source.
///
/// The discriminant (repr u16) IS the intrinsic ID stored in `SymbolEntry`.
/// 0xFFFF is reserved as "not an intrinsic" sentinel.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum Intrinsic {
    Writeln       = 0,
    Write         = 1,
    IntToString   = 2,
    FloatToString = 3,
    StringLength  = 4,
    NatToString   = 5,
}

impl Intrinsic {
    #[must_use]
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "writeln"         => Some(Self::Writeln),
            "write"           => Some(Self::Write),
            "int_to_string"   => Some(Self::IntToString),
            "float_to_string" => Some(Self::FloatToString),
            "string_length"   => Some(Self::StringLength),
            "nat_to_string"   => Some(Self::NatToString),
            _                 => None,
        }
    }

    #[must_use]
    pub const fn id(self) -> u16 {
        match self {
            Self::Writeln       => 0,
            Self::Write         => 1,
            Self::IntToString   => 2,
            Self::FloatToString => 3,
            Self::StringLength  => 4,
            Self::NatToString   => 5,
        }
    }

    #[must_use]
    pub const fn from_id(id: u16) -> Option<Self> {
        match id {
            0 => Some(Self::Writeln),
            1 => Some(Self::Write),
            2 => Some(Self::IntToString),
            3 => Some(Self::FloatToString),
            4 => Some(Self::StringLength),
            5 => Some(Self::NatToString),
            _ => None,
        }
    }

    #[must_use]
    pub const fn name(self) -> &'static str {
        match self {
            Self::Writeln       => "writeln",
            Self::Write         => "write",
            Self::IntToString   => "int_to_string",
            Self::FloatToString => "float_to_string",
            Self::StringLength  => "string_length",
            Self::NatToString   => "nat_to_string",
        }
    }
}

/// Sentinel stored in `SymbolEntry.intrinsic_id` for non-intrinsic functions.
pub const NONE_ID: u16 = 0xFFFF;
