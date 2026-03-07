// Algebraic types, pattern matching, and records.

import { sqrt } from "musi:math";

record Point { x: Float, y: Float };

choice Shape {
    Circle(Float)
  | Rect(Float, Float)
  | Triangle(Float, Float, Float)
};

fn area(s: Shape): Float =>
    match s with (
        .Circle(r)          => 3.141592653589793 * r * r
      | .Rect(w, h)         => w * h
      | .Triangle(a, b, c)  => (
            const s := (a + b + c) / 2.0;
            sqrt(s * (s - a) * (s - b) * (s - c))
        )
    );

fn describe(s: Shape): String =>
    match s with (
        .Circle(r)          => string_concat("circle r=", float_to_string(r))
      | .Rect(w, h)         => string_concat(float_to_string(w), string_concat("x", float_to_string(h)))
      | .Triangle(_, _, _)  => "triangle"
    );

const shapes := [.Circle(5.0), .Rect(4.0, 6.0), .Triangle(3.0, 4.0, 5.0)];

for s in shapes loop (
    writeln(string_concat(describe(s), string_concat(" area=", float_to_string(area(s)))))
);
