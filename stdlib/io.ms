unsafe extern "C" proc write(fd: Int, ptr: Ptr<const Unit>, len: Nat) -> Int;

export const writeln := proc (s: Str) {
  unsafe {
    write(1, s.pointee, s.len);
    write(1, "\n".pointee, 1);
  }
};
