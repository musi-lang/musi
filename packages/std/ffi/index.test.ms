let testing := import "@std/testing";
let ffi := import "@std/ffi";

export let test () :=
  (
    testing.describe("ffi");
    let pointer := ffi.ptr.null[Int]();
    let samePointer := unsafe { ffi.ptr.offset[Int](pointer, 0); };
    let offset := ffi.ptr.offset;
    let aliasedPointer := unsafe { offset[Int](pointer, 0); };
    testing.it("'ptr.null' creates null pointer", testing.toBeTruthy(ffi.ptr.isNull[Int](pointer)));
    testing.it("'ptr.offset' preserves null pointer at zero offset", testing.toBeTruthy(ffi.ptr.isNull[Int](samePointer)));
    testing.it("'ptr.offset' works as first-class field", testing.toBeTruthy(ffi.ptr.isNull[Int](aliasedPointer)));
    testing.endDescribe()
  );
