val Buf<T> := record {
  var data: [T];
  var len: Nat;
  var cap: Nat;
};

val new<T> := fn (cap: Nat) -> Buf<T> {
  Buf {
    .data := [T; cap],
    .len := 0,
    .cap := cap,
  }
};

val push<T> := fn (ref var buf: Buf<T>, item: T) {
  if buf.len >= buf.cap then {
    // TODO: resize Buf
    return;
  };
  buf.data[buf.len] <- item;
  buf.len <- buf.len + 1;
};

val get<T> := fn (ref buf: Buf<T>, idx: Nat) -> T {
  buf.data[idx]
};

val length<T> := fn (ref buf: Buf<T>) -> Nat {
  buf.len
};
