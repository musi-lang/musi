pub const STD_PACKAGE_NAME: &str = "@std";
pub const STD_MANIFEST_PATH: &str = "builtin:/@std/musi.json";
pub const STD_ROOT_DIR: &str = "builtin:/@std";
pub const STD_MANIFEST: &str = include_str!("../../../lib/std/musi.json");
pub const STD_FILES: &[(&str, &str)] = &[
    ("ascii.ms", include_str!("../../../lib/std/ascii.ms")),
    ("assert.ms", include_str!("../../../lib/std/assert.ms")),
    ("bits.ms", include_str!("../../../lib/std/bits.ms")),
    ("word.ms", include_str!("../../../lib/std/word.ms")),
    ("bytes.ms", include_str!("../../../lib/std/bytes.ms")),
    ("cli.ms", include_str!("../../../lib/std/cli.ms")),
    (
        "cli/prompt.ms",
        include_str!("../../../lib/std/cli/prompt.ms"),
    ),
    ("cmp.ms", include_str!("../../../lib/std/cmp.ms")),
    (
        "collections.ms",
        include_str!("../../../lib/std/collections.ms"),
    ),
    (
        "collections/array.ms",
        include_str!("../../../lib/std/collections/array.ms"),
    ),
    (
        "collections/list.ms",
        include_str!("../../../lib/std/collections/list.ms"),
    ),
    ("crypto.ms", include_str!("../../../lib/std/crypto.ms")),
    ("datetime.ms", include_str!("../../../lib/std/datetime.ms")),
    ("encoding.ms", include_str!("../../../lib/std/encoding.ms")),
    (
        "encoding/base64.ms",
        include_str!("../../../lib/std/encoding/base64.ms"),
    ),
    (
        "encoding/hex.ms",
        include_str!("../../../lib/std/encoding/hex.ms"),
    ),
    (
        "encoding/utf8.ms",
        include_str!("../../../lib/std/encoding/utf8.ms"),
    ),
    ("env.ms", include_str!("../../../lib/std/env.ms")),
    ("errors.ms", include_str!("../../../lib/std/errors.ms")),
    ("ffi.ms", include_str!("../../../lib/std/ffi.ms")),
    ("fmt.ms", include_str!("../../../lib/std/fmt.ms")),
    ("fs.ms", include_str!("../../../lib/std/fs.ms")),
    ("io.ms", include_str!("../../../lib/std/io.ms")),
    ("json.ms", include_str!("../../../lib/std/json.ms")),
    ("libc.ms", include_str!("../../../lib/std/libc.ms")),
    ("libm.ms", include_str!("../../../lib/std/libm.ms")),
    ("log.ms", include_str!("../../../lib/std/log.ms")),
    ("math.ms", include_str!("../../../lib/std/math.ms")),
    (
        "math/_float.ms",
        include_str!("../../../lib/std/math/_float.ms"),
    ),
    (
        "math/_integer.ms",
        include_str!("../../../lib/std/math/_integer.ms"),
    ),
    ("maybe.ms", include_str!("../../../lib/std/maybe.ms")),
    ("os.ms", include_str!("../../../lib/std/os.ms")),
    ("path.ms", include_str!("../../../lib/std/path.ms")),
    ("prelude.ms", include_str!("../../../lib/std/prelude.ms")),
    ("process.ms", include_str!("../../../lib/std/process.ms")),
    ("random.ms", include_str!("../../../lib/std/random.ms")),
    ("expect.ms", include_str!("../../../lib/std/expect.ms")),
    ("semver.ms", include_str!("../../../lib/std/semver.ms")),
    ("std.ms", include_str!("../../../lib/std/std.ms")),
    ("_sys.ms", include_str!("../../../lib/std/_sys.ms")),
    ("testing.ms", include_str!("../../../lib/std/testing.ms")),
    ("text.ms", include_str!("../../../lib/std/text.ms")),
    ("uuid.ms", include_str!("../../../lib/std/uuid.ms")),
];
