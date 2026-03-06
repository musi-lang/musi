/// Every built-in function callable from Musi source.
///
/// The discriminant (repr u16) IS the intrinsic ID stored in SymbolEntry.
/// 0xFFFF is reserved as "not an intrinsic" sentinel.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum Intrinsic {
    Writeln     = 0,
    Write       = 1,
    IntToString = 2,
}

impl Intrinsic {
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "writeln"       => Some(Self::Writeln),
            "write"         => Some(Self::Write),
            "int_to_string" => Some(Self::IntToString),
            _               => None,
        }
    }

    pub const fn id(self) -> u16 { self as u16 }

    pub fn from_id(id: u16) -> Option<Self> {
        match id {
            0 => Some(Self::Writeln),
            1 => Some(Self::Write),
            2 => Some(Self::IntToString),
            _ => None,
        }
    }

    pub const fn name(self) -> &'static str {
        match self {
            Self::Writeln     => "writeln",
            Self::Write       => "write",
            Self::IntToString => "int_to_string",
        }
    }
}

/// Sentinel stored in SymbolEntry.intrinsic_id for non-intrinsic functions.
pub const NONE_ID: u16 = 0xFFFF;
