/// Every built-in function callable from Musi source.
///
/// The discriminant (repr u16) IS the intrinsic ID stored in `SymbolEntry`.
/// 0xFFFF is reserved as "not an intrinsic" sentinel.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum Intrinsic {
    // -- legacy VM primitives (0–23) --
    Writeln        = 0,
    Write          = 1,
    IntToString    = 2,
    FloatToString  = 3,
    StringLength   = 4,
    NatToString    = 5,
    StringConcat   = 6,
    StringSlice    = 7,
    StringToInt    = 8,
    StringContains = 9,
    FloatSqrt      = 10,
    FloatPow       = 11,
    FloatFloor     = 12,
    FloatCeil      = 13,
    ReadLine       = 14,
    ArrayLength    = 15,
    ArrayPush      = 16,
    ArrayPop       = 17,
    ArrayGet       = 18,
    ArraySet       = 19,
    ArraySlice     = 20,
    Assert         = 21,
    AssertMsg      = 22,
    Test           = 23,

    // -- musi:fs (24–31) --
    FsReadFile   = 24,
    FsWriteFile  = 25,
    FsAppendFile = 26,
    FsReadDir    = 27,
    FsExists     = 28,
    FsRemove     = 29,
    FsRename     = 30,
    FsMakeDir    = 31,

    // -- musi:path (32–38) --
    PathJoin       = 32,
    PathDirname    = 33,
    PathBasename   = 34,
    PathExtension  = 35,
    PathIsAbsolute = 36,
    PathNormalize  = 37,
    PathStem       = 38,

    // -- musi:os (39–44) --
    OsPlatform = 39,
    OsArch     = 40,
    OsHomeDir  = 41,
    OsHostname = 42,
    OsCpuCount = 43,
    OsTmpDir   = 44,

    // -- musi:process (45–48) --
    ProcessArgs   = 45,
    ProcessEnvGet = 46,
    ProcessExit   = 47,
    ProcessCwd    = 48,

    // -- musi:time (49–50) --
    TimeNowMillis = 49,
    TimeNowSecs   = 50,
}

/// Name strings indexed by variant discriminant.
const NAMES: &[&str] = &[
    // 0–23
    "writeln", "write", "int_to_string", "float_to_string",
    "string_length", "nat_to_string", "string_concat", "string_slice",
    "string_to_int", "string_contains", "float_sqrt", "float_pow",
    "float_floor", "float_ceil", "read_line",
    "array_length", "array_push", "array_pop", "array_get", "array_set",
    "array_slice",
    "assert", "assert_msg", "test",
    // 24–31  musi:fs
    "fs_read_file", "fs_write_file", "fs_append_file", "fs_read_dir",
    "fs_exists", "fs_remove", "fs_rename", "fs_make_dir",
    // 32–38  musi:path
    "path_join", "path_dirname", "path_basename", "path_extension",
    "path_is_absolute", "path_normalize", "path_stem",
    // 39–44  musi:os
    "os_platform", "os_arch", "os_home_dir", "os_hostname",
    "os_cpu_count", "os_tmp_dir",
    // 45–48  musi:process
    "process_args", "process_env_get", "process_exit", "process_cwd",
    // 49–50  musi:time
    "time_now_millis", "time_now_secs",
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
            24 => Some(Self::FsReadFile),
            25 => Some(Self::FsWriteFile),
            26 => Some(Self::FsAppendFile),
            27 => Some(Self::FsReadDir),
            28 => Some(Self::FsExists),
            29 => Some(Self::FsRemove),
            30 => Some(Self::FsRename),
            31 => Some(Self::FsMakeDir),
            32 => Some(Self::PathJoin),
            33 => Some(Self::PathDirname),
            34 => Some(Self::PathBasename),
            35 => Some(Self::PathExtension),
            36 => Some(Self::PathIsAbsolute),
            37 => Some(Self::PathNormalize),
            38 => Some(Self::PathStem),
            39 => Some(Self::OsPlatform),
            40 => Some(Self::OsArch),
            41 => Some(Self::OsHomeDir),
            42 => Some(Self::OsHostname),
            43 => Some(Self::OsCpuCount),
            44 => Some(Self::OsTmpDir),
            45 => Some(Self::ProcessArgs),
            46 => Some(Self::ProcessEnvGet),
            47 => Some(Self::ProcessExit),
            48 => Some(Self::ProcessCwd),
            49 => Some(Self::TimeNowMillis),
            50 => Some(Self::TimeNowSecs),
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
