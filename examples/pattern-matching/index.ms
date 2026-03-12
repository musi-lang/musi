let Shape := choice {
    Circle of Float64
  + Rect of (Float64, Float64)
  + Point
};

let area : (Shape) -> Float64 :=
    (shape) => match shape (
        .Circle(r) => 3.14159 * r * r
      | .Rect(w, h) => w * h
      | .Point => 0.0
    );

let describe : (Shape) -> String :=
    (shape) => match shape (
        .Circle(r)  => f"circle with radius {r}"
      | .Rect(w, h) => f"rectangle {w} x {h}"
      | .Point      => "a point"
    );

#[entrypoint]
let main : () ~> () under { IO } :=
    () => (
        let c := area(.Circle(5.0));
        writeln(f"circle area = {c}");
        let r := area(.Rect(3.0, 4.0));
        writeln(f"rect area = {r}");
        writeln(describe(.Point))
    );
