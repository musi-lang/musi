extern "C" {
  fn fopen(path: Ptr<Unit>, mode: Ptr<Unit>) -> Ptr<Unit>;
  fn fclose(file: Ptr<Unit>) -> Int;
  fn fread(buf: Ptr<Unit>, size: Nat, count: Nat, file: Ptr<Unit>) -> Nat;
  fn fwrite(buf: Ptr<Unit>, size: Nat, count: Nat, file: Ptr<Unit>) -> Nat;
};

val read_file := fn(path: Ptr<Unit>, buffer: Ptr<Unit>, size: Nat) -> Nat {
  val file := fopen(path, path);
  val bytes_read := fread(buffer, 1, size, file);
  fclose(file);
  bytes_read
};

val write_file := fn(path: Ptr<Unit>, data: Ptr<Unit>, size: Nat) -> Nat {
  val file := fopen(path, path);
  val bytes_written := fwrite(data, 1, size, file);
  fclose(file);
  bytes_written
};
