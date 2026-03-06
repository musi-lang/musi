// a.max(b) routes through given Ord[Int]; a.show() through given Show[Int].

const a := 3;
const b := 7;

writeln(int_to_string(a.max(b)));  // 7
writeln(int_to_string(a.min(b)));  // 3

// eq via type class
const same := a.eq(a);
const diff := a.eq(b);
writeln(int_to_string(if same then 1 else 0));  // 1
writeln(int_to_string(if diff then 1 else 0));  // 0
