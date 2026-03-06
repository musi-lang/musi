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
    ArrayLength   = 15,
    ArrayPush     = 16,
    ArrayPop      = 17,
    ArrayGet      = 18,
    ArraySet      = 19,
    ArraySlice    = 20,
    Assert        = 21,
    AssertMsg     = 22,
    Test          = 23,
}

/// Name strings indexed by variant discriminant.
const NAMES: &[&str] = &[
    "writeln", "write", "int_to_string", "float_to_string",
    "string_length", "nat_to_string", "string_concat", "string_slice",
    "string_to_int", "string_contains", "float_sqrt", "float_pow",
    "float_floor", "float_ceil", "read_line",
    "array_length", "array_push", "array_pop", "array_get", "array_set",
    "array_slice",
    "assert", "assert_msg", "test",
];

impl Intrinsic {
    #[must_use]
    pub fn from_name(name: &str) -> Option<Self> {
        let pos = NAMES.iter().position(|&n| n == name)?;
        Self::from_id(u16::try_from(pos).ok()?)
    }

    #[must_use]
    pub const fn id(self) -> u16 {
        self as u16
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
            15 => Some(Self::ArrayLength),
            16 => Some(Self::ArrayPush),
            17 => Some(Self::ArrayPop),
            18 => Some(Self::ArrayGet),
            19 => Some(Self::ArraySet),
            20 => Some(Self::ArraySlice),
            21 => Some(Self::Assert),
            22 => Some(Self::AssertMsg),
            23 => Some(Self::Test),
            _  => None,
        }
    }

    #[must_use]
    pub fn name(self) -> &'static str {
        NAMES[usize::from(self.id())]
    }
}

/// Sentinel stored in `SymbolEntry.intrinsic_id` for non-intrinsic functions.
pub const NONE_ID: u16 = 0xFFFF;
