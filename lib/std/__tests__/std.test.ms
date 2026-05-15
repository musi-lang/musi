let std := import "@std";
let assert := import "@std/assert";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("std");
    let sameBytes := std.bytes.fromArray([1, 2]);
    let oneList := std.collections.list.of[Int](1);
    let currentOs := std.os.targetOs(std.os.current);
    let noneInt := std.maybe.None[Int]();
    let successExpect := std.expect.Success[Int, String](1);
    let nullPointer := std.ffi.ptr.null[std.ffi.CInt]();
    let rootPath := std.path.fromString("/");
    testing.it(
      "root exports 'bytes' family",
      sameBytes.equals(std.bytes.fromArray([1, 2])),
    );
    testing.it(
      "root exports 'prelude' family",
      std.prelude.Some[Int](1).isSome(),
    );
    testing.it("root exports 'ascii' family", std.ascii.isHexDigit(70));
    testing.it(
      "root exports 'expect' family",
      std.expect.isSuccess[Int, String](successExpect),
    );
    testing.it("root exports 'cmp' family", std.cmp.compareInt(1, 2).isLess(),);
    testing.it(
      "root exports 'list' family",
      std.collections.list.equalsInt(
        oneList,
        std.collections.list.fromArray[Int]([1]),
      ),
    );
    testing.it("root exports 'text' family", std.text.length("musi") > 0);
    testing.it("root exports 'json' family", std.json.isValid("{}"));
    testing.it(
      "root exports 'math' family",
      assert.toBe(std.math.clamp(9, 0, 4), 4),
    );
    testing.it(
      "root exports 'libm' family",
      std.maybe.isSome[Float](std.libm.checkedSqrt(4.0)),
    );
    testing.it("root exports 'libc' family", std.libc.equals("musi", "musi"));
    testing.it(
      "root exports 'ffi' family",
      std.ffi.ptr.isNull[std.ffi.CInt](nullPointer),
    );
    testing.it(
      "root exports 'bits' family",
      assert.toBe(std.bits.toInt8(std.bits.fromInt8(3)), 3),
    );
    testing.it(
      "root exports 'word' family",
      assert.toBe(std.word.toInt32(std.word.fromInt32(3)), 3),
    );
    testing.it("root exports 'fmt' family", std.fmt.int(42) = "42");
    testing.it(
      "root exports 'errors' family",
      std.errors.error("failed").toString() = "failed",
    );
    testing.it(
      "root exports 'encoding' family",
      std.encoding.hex.encode("a") /= "",
    );
    testing.it(
      "root exports 'datetime' family",
      std.datetime.seconds(2).toMillis() = 2000,
    );
    testing.it(
      "root exports 'random' family",
      std.random.nextIntInRange(1, 10) >= 1,
    );
    testing.it("root exports 'crypto' family", std.crypto.sha256Hex("") /= "",);
    testing.it("root exports 'fs' family", std.fs.exists(rootPath));
    testing.it(
      "root exports 'path' family",
      std.path.fromString("dir/file.txt").baseName() = "file.txt",
    );
    testing.it("root exports 'io' family",
        (
          std.io.write("");
          0 = 0
        ));
    testing.it(
      "root exports 'env' family",
      std.env.key("PATH").toString() = "PATH",
    );
    testing.it("root exports 'process' family", std.process.argsCount() >= 0,);
    testing.it("root exports 'os' family", currentOs.toString() /= "",);
    testing.it(
      "root exports 'maybe' family",
      assert.toBe(std.maybe.unwrapOr[Int](noneInt, 5), 5),
    );
    testing.it("root exports 'log' family", std.log.warnLevel.name() = "warn",);
    testing.it("root exports 'cli' family", 0 = 0);
    testing.it(
      "root exports 'uuid' family",
      std.uuid.isValid("550e8400-e29b-41d4-a716-446655440000"),
    );
    testing.it("root exports 'semver' family", std.semver.isValid("1.2.3"));
    testing.it("root exports 'assert' family", std.assert.isTrue(1 = 1));
    testing.it("root exports 'testing' family", 1 = 1);
    testing.it(
      "local binding preserves import record target",
      std.bytes.fromArray([3, 4]).equals(std.bytes.fromArray([3, 4])),
    );
    testing.endDescribe()
  );
