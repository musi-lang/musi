extern {
  @link(prefix := "__builtin_")
  fn write(fd: Int, s: Str) -> Int;
};

val writeln := fn (s: Str) {
  __builtin_write(1, s);
};

val write := fn (s: Str) {
  __builtin_write(1, s);
};

export { writeln, write };
