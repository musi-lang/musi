import { StrBuf } from "@std/mem";

val len := fn(ref s: StrBuf) -> Nat {
  // TODO: Implement string length
  0
};

val concat := fn(ref a: StrBuf, ref b: StrBuf) -> StrBuf {
  // TODO: Implement string concatenation
  StrBuf{}
};

val substr := fn(ref s: StrBuf, start: Nat, end: Nat) -> StrBuf {
  // TODO: Implement substring
  StrBuf{}
};

export { strlen, concat, substr };