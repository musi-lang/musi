@link("musi.io.write")
const io_write := unsafe extern "C" proc (s: Text);

@link("musi.io.writeln")
const io_writeln := unsafe extern "C" proc (s: Text);

/// Writes given text followed by newline to standard output.
const write := proc (s: Text) {
  return io_write(s);
};

/// Writes given text to standard output.
const writeln := proc (s: Text) {
  return io_writeln(s);
};

export { write, writeln };
