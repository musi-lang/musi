@link("musi.io.write")
/// Writes given text followed by newline to standard output.
unsafe extern "C" proc io_write(s: Text);

@link("musi.io.writeln")
/// Writes given text to standard output.
unsafe extern "C" proc io_writeln(s: Text);

const write := proc (s: Text) {
  return io_write(s);
};

const writeln := proc (s: Text) {
  return io_writeln(s);
};

export { write, writeln };
