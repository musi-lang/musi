// Record types: creation, field access, and functional update
record Point { x: Int, y: Int };

const p1 := Point.{ x := 0, y := 0 };
const p2 := Point.{ x := 3, y := 4 };

// Field access
writeln(int_to_string(p2.x));
writeln(int_to_string(p2.y));

// Functional update via spread
const p3 := Point.{ <..p2, x := 10 };
writeln(int_to_string(p3.x));
writeln(int_to_string(p3.y));
