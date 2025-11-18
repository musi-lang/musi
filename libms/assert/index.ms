extern {
  @link(prefix := "__builtin_")
  fn panic(ref msg: StrBuf) -> !;
};

val assert := fn(cond: Bool, ref msg: StrBuf) {
  if not cond then __builtin_panic(msg);
};

val assert_eq := fn<T>(lhs: T, rhs: T, ref msg: StrBuf) {
  if lhs =/= rhs then __builtin_panic(msg);
};

val assert_ne := fn<T>(lhs: T, rhs: T, ref msg: StrBuf) {
  if lhs = rhs then __builtin_panic(msg);
};

export { assert, assert_eq, assert_ne };
