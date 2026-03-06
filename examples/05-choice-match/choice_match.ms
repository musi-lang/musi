// Algebraic data types (choice) and pattern matching
choice Shape {
    Circle(Int)
  | Rect(Int, Int)
  | Triangle(Int, Int)
};

fn area(s: Shape): Int => match s with (
    Circle(r)      => r * r
  | Rect(w, h)     => w * h
  | Triangle(b, h) => b * h / 2
);

writeln(int_to_string(area(Circle(5))));
writeln(int_to_string(area(Rect(4, 6))));
writeln(int_to_string(area(Triangle(3, 8))));
