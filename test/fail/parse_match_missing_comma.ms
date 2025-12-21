fn area(s: Shape): Bin32 {
  match s {
  case Circle(r) => 3.14159 * r ** 2
  case Rect(w, h) => w * h
  case Triangle(a, b, c) => {
    val s := (a + b + c) / 2.0;
    return sqrt(s * (s - a) * (s - b) * (s - c));
  }
  }
};
