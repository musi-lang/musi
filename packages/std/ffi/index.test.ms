let Testing := import "@std/testing";
let Ffi := import "@std/ffi";

export let test () :=
  (
    Testing.describe("ffi");
    let pointer := Ffi.ptr.null[Int]();
    let samePointer := unsafe { Ffi.ptr.offset[Int](pointer, 0); };
    let offset := Ffi.ptr.offset;
    let aliasedPointer := unsafe { offset[Int](pointer, 0); };
    Testing.it("'ptr.null' creates null pointer", Testing.toBeTruthy(Ffi.ptr.isNull[Int](pointer)));
    Testing.it("'ptr.offset' preserves null pointer at zero offset", Testing.toBeTruthy(Ffi.ptr.isNull[Int](samePointer)));
    Testing.it("'ptr.offset' works as first-class field", Testing.toBeTruthy(Ffi.ptr.isNull[Int](aliasedPointer)));
    Testing.endDescribe()
  );
