const __builtin_write := unsafe extern proc (fd: Int, s: Str) -> Int;

export const writeln := proc (s: Str) {
  unsafe {
    __builtin_write(1, s);
    __builtin_write(1, "\n");
  }
};

export const write := proc (s: Str) {
  unsafe {
    __builtin_write(1, s);
  }
};
