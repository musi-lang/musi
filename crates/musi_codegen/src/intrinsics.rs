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
    StringConcat  = 6,
    StringSlice   = 7,
    StringToInt   = 8,
    StringContains = 9,
    FloatSqrt     = 10,
    FloatPow      = 11,
    FloatFloor    = 12,
    FloatCeil     = 13,
    ReadLine      = 14,
}

impl Intrinsic {
    #[must_use]
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "writeln"          => Some(Self::Writeln),
            "write"            => Some(Self::Write),
            "int_to_string"    => Some(Self::IntToString),
            "float_to_string"  => Some(Self::FloatToString),
            "string_length"    => Some(Self::StringLength),
            "nat_to_string"    => Some(Self::NatToString),
            "string_concat"    => Some(Self::StringConcat),
            "string_slice"     => Some(Self::StringSlice),
            "string_to_int"    => Some(Self::StringToInt),
            "string_contains"  => Some(Self::StringContains),
            "float_sqrt"       => Some(Self::FloatSqrt),
            "float_pow"        => Some(Self::FloatPow),
            "float_floor"      => Some(Self::FloatFloor),
            "float_ceil"       => Some(Self::FloatCeil),
            "read_line"        => Some(Self::ReadLine),
            _                  => None,
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
            Self::StringConcat  => 6,
            Self::StringSlice   => 7,
            Self::StringToInt   => 8,
            Self::StringContains => 9,
            Self::FloatSqrt     => 10,
            Self::FloatPow      => 11,
            Self::FloatFloor    => 12,
            Self::FloatCeil     => 13,
            Self::ReadLine      => 14,
        }
    }

    #[must_use]
    pub const fn from_id(id: u16) -> Option<Self> {
        match id {
            0  => Some(Self::Writeln),
            1  => Some(Self::Write),
            2  => Some(Self::IntToString),
            3  => Some(Self::FloatToString),
            4  => Some(Self::StringLength),
            5  => Some(Self::NatToString),
            6  => Some(Self::StringConcat),
            7  => Some(Self::StringSlice),
            8  => Some(Self::StringToInt),
            9  => Some(Self::StringContains),
            10 => Some(Self::FloatSqrt),
            11 => Some(Self::FloatPow),
            12 => Some(Self::FloatFloor),
            13 => Some(Self::FloatCeil),
            14 => Some(Self::ReadLine),
            _  => None,
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
            Self::StringConcat  => "string_concat",
            Self::StringSlice   => "string_slice",
            Self::StringToInt   => "string_to_int",
            Self::StringContains => "string_contains",
            Self::FloatSqrt     => "float_sqrt",
            Self::FloatPow      => "float_pow",
            Self::FloatFloor    => "float_floor",
            Self::FloatCeil     => "float_ceil",
            Self::ReadLine      => "read_line",
        }
    }
}

/// Sentinel stored in `SymbolEntry.intrinsic_id` for non-intrinsic functions.
pub const NONE_ID: u16 = 0xFFFF;
