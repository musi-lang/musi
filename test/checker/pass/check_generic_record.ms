val Buf<T> := record {
  var data: [T],
  var len: Nat
};

val buf_int: Buf<Int> := Buf{ .data := [1, 2, 3], .len := 3 };
val buf_str: Buf<Str> := Buf{ .data := ["a", "b"], .len := 2 };
