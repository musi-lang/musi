/// Generates the `Intrinsic` enum and all lookup methods from a single
/// declarative source. Adding a new intrinsic is one line here — nothing else.
///
/// Each entry: `<id> => <Variant> "<name>"` where `id` is the stable u16
/// discriminant stored in bytecode, and `name` is the string the compiler
/// writes and the registry reads.
macro_rules! define_intrinsics {
    ( $( $id:literal => $variant:ident $name:literal ),* $(,)? ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[repr(u16)]
        pub enum Intrinsic {
            $( $variant = $id, )*
        }

        impl Intrinsic {
            /// Look up by name string. O(1) — compiled to a jump table.
            #[must_use]
            pub fn from_name(name: &str) -> Option<Self> {
                match name {
                    $( $name => Some(Self::$variant), )*
                    _ => None,
                }
            }

            /// Look up by numeric ID. O(1) — compiled to a jump table.
            #[must_use]
            pub const fn from_id(id: u16) -> Option<Self> {
                match id {
                    $( $id => Some(Self::$variant), )*
                    _ => None,
                }
            }

            /// Canonical name for this intrinsic (same string used in source).
            #[must_use]
            pub const fn name(self) -> &'static str {
                match self {
                    $( Self::$variant => $name, )*
                }
            }

            /// Numeric ID (equals the repr discriminant).
            #[must_use]
            #[allow(clippy::as_conversions)] // repr(u16) cast is always exact
            pub const fn id(self) -> u16 {
                self as u16
            }
        }
    };
}

define_intrinsics! {
    // -- core prelude (VM fallback; always available without import) --
     0 => Writeln        "writeln",
     1 => Write          "write",
     2 => IntToString    "int_to_string",
     3 => FloatToString  "float_to_string",
     4 => StringLength   "string_length",    // → musi:string
     5 => NatToString    "nat_to_string",    // → musi:string
     6 => StringConcat   "string_concat",
     7 => StringSlice    "string_slice",     // → musi:string
     8 => StringToInt    "string_to_int",    // → musi:string
     9 => StringContains "string_contains",  // → musi:string
    10 => Sqrt           "sqrt",             // → musi:math
    11 => Pow            "pow",              // → musi:math
    12 => Floor          "floor",            // → musi:math
    13 => Ceil           "ceil",             // → musi:math
    14 => ReadLine       "read_line",        // → musi:io
    15 => ArrayLength    "array_length",
    16 => ArrayPush      "array_push",
    17 => ArrayPop       "array_pop",
    18 => ArrayGet       "array_get",
    19 => ArraySet       "array_set",
    20 => ArraySlice     "array_slice",
    21 => Assert         "assert",
    22 => AssertMsg      "assert_msg",
    23 => Test           "test",
    // -- musi:fs --
    24 => FsReadFile     "fs_read_file",
    25 => FsWriteFile    "fs_write_file",
    26 => FsAppendFile   "fs_append_file",
    27 => FsReadDir      "fs_read_dir",
    28 => FsExists       "fs_exists",
    29 => FsRemove       "fs_remove",
    30 => FsRename       "fs_rename",
    31 => FsMakeDir      "fs_make_dir",
    // -- musi:path --
    32 => PathJoin       "path_join",
    33 => PathDirname    "path_dirname",
    34 => PathBasename   "path_basename",
    35 => PathExtension  "path_extension",
    36 => PathIsAbsolute "path_is_absolute",
    37 => PathNormalize  "path_normalize",
    38 => PathStem       "path_stem",
    // -- musi:os --
    39 => OsPlatform     "os_platform",
    40 => OsArch         "os_arch",
    41 => OsHomeDir      "os_home_dir",
    42 => OsHostname     "os_hostname",
    43 => OsCpuCount     "os_cpu_count",
    44 => OsTmpDir       "os_tmp_dir",
    // -- musi:process --
    45 => ProcessArgs    "process_args",
    46 => ProcessEnvGet  "process_env_get",
    47 => ProcessExit    "process_exit",
    48 => ProcessCwd     "process_cwd",
    // -- musi:time --
    49 => TimeNowMillis  "time_now_millis",
    50 => TimeNowSecs    "time_now_secs",
    // -- musi:math extended --
    51 => Round          "round",
    52 => Fabs           "fabs",
    53 => Fmin           "fmin",
    54 => Fmax           "fmax",
    55 => Fclamp         "fclamp",
    // -- string extended --
    56 => StringSplit    "string_split",
    57 => StringTrim     "string_trim",
    58 => StringToLower  "string_to_lower",
    59 => StringToUpper  "string_to_upper",
    60 => StringToFloat  "string_to_float",
    61 => StringIndexOf  "string_index_of",
    // -- numeric casts --
    62 => IntToFloat     "int_to_float",
    63 => FloatToInt     "float_to_int",
    // -- math trig --
    64 => Sin            "sin",
    65 => Cos            "cos",
    66 => Tan            "tan",
    67 => Atan2          "atan2",
    68 => Log            "log",
    69 => Exp            "exp",
    // -- rune --
    78 => RuneToString   "rune_to_string",
    79 => StringToRunes  "string_to_runes",
    80 => RuneToInt      "rune_to_int",
    81 => IntToRune      "int_to_rune",
    // -- map --
    70 => MapNew         "map_new",
    71 => MapGet         "map_get",
    72 => MapSet         "map_set",
    73 => MapHas         "map_has",
    74 => MapDelete      "map_delete",
    75 => MapKeys        "map_keys",
    76 => MapValues      "map_values",
    77 => MapLen         "map_len",
}

/// Sentinel stored in `SymbolEntry.intrinsic_id` for non-intrinsic functions.
pub const NONE_ID: u16 = 0xFFFF;
