// we'd be defining writeln and write here
// so, my question is... is it possible without intrinsics?
// or do we need intrinsics for that?
// hmm... i guess we need extern FFI, no?
// call from Rust's stdio?
// extern "Rust" or uisng Rust's libc binding to connect extern "C" or what?

export const write := proc (s: Str) {
  // extern call to Rust's stdio write function?
  // hmm... how to do that?
  // or maybe we can just use C's stdio?
  // wait, maybe we need to check if we can use C's stdio directly
  // rust thing down here:
  // ```
  // extern "C" {
  //   fn printf(fmt: *const u8, ...) -> i32;
  // }
  // ```
  // hmm... need to figure this out somehow... FUUUUUUUCK.
};

// implicit effing '-> Unit' because it's okay for unit type to be inferred
// same as writing `-> ()` anyway, and like Rust, we can omit it
export const writeln := proc (s: Str) {
  write(s + "\n"); // wait, will this be allowed?
  // or should we call rust's native println function directly?
};
