extern "C" {
  proc fopen(path: Ptr<Unit>, mode: Ptr<Unit>) -> Ptr<Unit>;
  proc fclose(file: Ptr<Unit>) -> Int;
  proc fread(buf: Ptr<Unit>, size: Nat, count: Nat, file: Ptr<Unit>) -> Nat;
  proc fwrite(buf: Ptr<Unit>, size: Nat, count: Nat, file: Ptr<Unit>) -> Nat;
};

val read_file := proc(path: Ptr<Unit>, buffer: Ptr<Unit>, size: Nat) -> Nat {
  val file := fopen(path, path);
  val  bytes_read := fread(buffer, 1, size, file);
  fclose(file);
  bytes_read
};

val write_file := proc(path: Ptr<Unit>, data: Ptr<Unit>, size: Nat) -> Nat {
  val file := fopen(path, path);
  val bytes_written := fwrite(data, 1, size, file);
  fclose(file);
  bytes_written
};
