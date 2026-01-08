/// Basic variable binding tests for semantic tokens
/// Tests val (readonly) vs var (mutable) distinction

val immutable := 42;
var mutable := 0;

val a := immutable + 1;
mutable <- mutable + a;

val result := immutable * mutable;
