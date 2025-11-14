@extern("C") unsafe proc fopen(path: Ptr<Unit>, mode: Ptr<Unit>) -> Ptr<Unit>;
@extern("C") unsafe proc fclose(file: Ptr<Unit>) -> Int;
@extern("C") unsafe proc fread(buf: Ptr<Unit>, size: Nat, count: Nat, file: Ptr<Unit>) -> Nat;
@extern("C") unsafe proc fwrite(buf: Ptr<Unit>, size: Nat, count: Nat, file: Ptr<Unit>) -> Nat;

const read_file := proc (path: Ptr<Unit>, buffer: Ptr<Unit>, size: Nat) -> Nat {
  unsafe {
    const file := fopen(path, path);
    const bytes_read := fread(buffer, 1, size, file);
    fclose(file);
    bytes_read
  }
};

const write_file := proc (path: Ptr<Unit>, data: Ptr<Unit>, size: Nat) -> Nat {
  unsafe {
    const file := fopen(path, path);
    const bytes_written := fwrite(data, 1, size, file);
    fclose(file);
    bytes_written
  }
};
