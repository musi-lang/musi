let bits := import "@std/bits";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("bits");
    let a := bits.fromInt8(10);
    let b := bits.fromInt8(12);
    testing.it(
      "fromNat8 converts natural values",
      testing.toBe(bits.toInt8(bits.fromNat8(10 :?> Nat)), 10),
    );
    testing.it(
      "fromInt8 converts integer values",
      testing.toBe(bits.toInt8(bits.fromInt8(7)), 7),
    );
    testing.it("zero8 builds empty bits", bits.isZero8(bits.zero8()));
    testing.it("ones8 builds full bits", bits.all8(bits.ones8()));
    testing.it("any8 detects set bits", bits.any8(a));
    testing.it("equals detects equal bits", bits.equals8(a, bits.fromInt8(10)));
    testing.it(
      "logicalAnd combines pointwise",
      testing.toBe(bits.toInt8(bits.logicalAnd8(a, b)), 8),
    );
    testing.it(
      "logicalOr combines pointwise",
      testing.toBe(bits.toInt8(bits.logicalOr8(a, b)), 14),
    );
    testing.it(
      "logicalXor combines pointwise",
      testing.toBe(bits.toInt8(bits.logicalXor8(a, b)), 6),
    );
    testing.it(
      "logicalNot inverts within width",
      testing.toBe(bits.toInt8(bits.logicalNot8(a)), 245),
    );
    testing.endDescribe()
  );
