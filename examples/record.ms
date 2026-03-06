record Point { x: Int, y: Int };
const p := Point.{ x := 1, y := 2 };
const q := Point.{ <..p, x := 99 };
writeln(int_to_string(p.x + p.y));
writeln(int_to_string(q.x));
writeln(int_to_string(q.y));
