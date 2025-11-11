unsafe extern "C" proc write(fd: Int, ptr: Ptr<const Unit>, len: Nat) -> Int;

export const writeln := proc (s: Str) {
  unsafe {
    write(1, s.ptr, s.len);
    write(1, "\n".ptr, 1);
  }
};
