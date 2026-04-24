let ffi := import "@std/ffi";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("ffi");
    let pointer := ffi.ptr.null[Int]();
    let samePointer := unsafe {
      ffi.ptr.offset[Int](pointer, 0);
    };
    let offset := ffi.ptr.offset;
    let aliasedPointer := unsafe {
      offset[Int](pointer, 0);
    };
    testing.it("'ptr.null' creates null pointer", ffi.ptr.isNull[Int](pointer),);
    testing.it(
      "'ptr.offset' preserves null pointer at zero offset",
      ffi.ptr.isNull[Int](samePointer),
    );
    testing.it(
      "'ptr.offset' works as stored field value",
      ffi.ptr.isNull[Int](aliasedPointer),
    );
    testing.endDescribe()
  );
