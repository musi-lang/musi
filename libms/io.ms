extern {
  @link(prefix := "__builtin_")
  fn write(fd: Int, s: Str) -> Int;
};

export val writeln := fn (s: Str) {
  __builtin_write(1, s);
};

export val write := fn (s: Str) {
  __builtin_write(1, s);
};
