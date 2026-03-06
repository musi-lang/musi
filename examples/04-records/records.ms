record Point { x: Int, y: Int };

const p1 := Point.{ x := 0, y := 0 };
const p2 := Point.{ x := 3, y := 4 };

writeln(int_to_string(p2.x));
writeln(int_to_string(p2.y));

const p3 := Point.{ <..p2, x := 10 };
writeln(int_to_string(p3.x));
writeln(int_to_string(p3.y));
