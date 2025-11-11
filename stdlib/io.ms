const __builtin_write := proc (fd: Int, ptr: Ptr<const Unit>, len: Nat) -> Int {
  // VM intrinsic at `proc_id 0`
  0
};

export const writeln := proc (s: Str) {
  __builtin_write(1, s.ptr, s.len);
  __builtin_write(1, "\n".ptr, 1);
};
