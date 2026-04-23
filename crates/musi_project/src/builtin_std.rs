pub const STD_PACKAGE_NAME: &str = "@std";
pub const STD_MANIFEST_PATH: &str = "builtin:/@std/musi.json";
pub const STD_ROOT_DIR: &str = "builtin:/@std";
pub const STD_MANIFEST: &str = include_str!("../../../packages/std/musi.json");
pub const STD_FILES: &[(&str, &str)] = &[
    ("assert.ms", include_str!("../../../packages/std/assert.ms")),
    ("bits.ms", include_str!("../../../packages/std/bits.ms")),
    ("bytes.ms", include_str!("../../../packages/std/bytes.ms")),
    ("cli.ms", include_str!("../../../packages/std/cli.ms")),
    (
        "cli/prompt.ms",
        include_str!("../../../packages/std/cli/prompt.ms"),
    ),
    ("cmp.ms", include_str!("../../../packages/std/cmp.ms")),
    (
        "collections.ms",
        include_str!("../../../packages/std/collections.ms"),
    ),
    (
        "collections/array.ms",
        include_str!("../../../packages/std/collections/array.ms"),
    ),
    (
        "collections/iter.ms",
        include_str!("../../../packages/std/collections/iter.ms"),
    ),
    (
        "collections/list.ms",
        include_str!("../../../packages/std/collections/list.ms"),
    ),
    (
        "collections/slice.ms",
        include_str!("../../../packages/std/collections/slice.ms"),
    ),
    ("crypto.ms", include_str!("../../../packages/std/crypto.ms")),
    (
        "datetime.ms",
        include_str!("../../../packages/std/datetime.ms"),
    ),
    (
        "encoding.ms",
        include_str!("../../../packages/std/encoding.ms"),
    ),
    (
        "encoding/base64.ms",
        include_str!("../../../packages/std/encoding/base64.ms"),
    ),
    (
        "encoding/hex.ms",
        include_str!("../../../packages/std/encoding/hex.ms"),
    ),
    (
        "encoding/utf8.ms",
        include_str!("../../../packages/std/encoding/utf8.ms"),
    ),
    ("env.ms", include_str!("../../../packages/std/env.ms")),
    ("errors.ms", include_str!("../../../packages/std/errors.ms")),
    ("ffi.ms", include_str!("../../../packages/std/ffi.ms")),
    ("fmt.ms", include_str!("../../../packages/std/fmt.ms")),
    ("fs.ms", include_str!("../../../packages/std/fs.ms")),
    ("io.ms", include_str!("../../../packages/std/io.ms")),
    ("json.ms", include_str!("../../../packages/std/json.ms")),
    ("log.ms", include_str!("../../../packages/std/log.ms")),
    ("math.ms", include_str!("../../../packages/std/math.ms")),
    (
        "math/float.ms",
        include_str!("../../../packages/std/math/float.ms"),
    ),
    (
        "math/integer.ms",
        include_str!("../../../packages/std/math/integer.ms"),
    ),
    ("option.ms", include_str!("../../../packages/std/option.ms")),
    ("os.ms", include_str!("../../../packages/std/os.ms")),
    ("path.ms", include_str!("../../../packages/std/path.ms")),
    (
        "prelude.ms",
        include_str!("../../../packages/std/prelude.ms"),
    ),
    (
        "process.ms",
        include_str!("../../../packages/std/process.ms"),
    ),
    ("random.ms", include_str!("../../../packages/std/random.ms")),
    ("result.ms", include_str!("../../../packages/std/result.ms")),
    ("semver.ms", include_str!("../../../packages/std/semver.ms")),
    ("std.ms", include_str!("../../../packages/std/std.ms")),
    ("sys.ms", include_str!("../../../packages/std/sys.ms")),
    (
        "testing.ms",
        include_str!("../../../packages/std/testing.ms"),
    ),
    ("text.ms", include_str!("../../../packages/std/text.ms")),
    ("uuid.ms", include_str!("../../../packages/std/uuid.ms")),
];
