extern {
  @link(prefix := "__builtin_")
  proc write(fd: Int, s: Str) -> Int;
};

export val writeln := proc (s: Str) {
  __builtin_write(1, s);
};

export val write := proc (s: Str) {
  __builtin_write(1, s);
};
