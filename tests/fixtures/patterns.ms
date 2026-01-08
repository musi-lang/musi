/// Pattern matching tests
/// Tests destructuring, guards, and various pattern forms

choice Shape {
  case Circle(Int),
  case Rectangle(Int, Int),
  case Triangle(Int, Int, Int),
};

fn area(shape: Shape): Int {
  match shape {
  case Circle(r) => 3 * r * r,
  case Rectangle(w, h) => w * h,
  case Triangle(a, b, c) => {
    val s := (a + b + c) / 2;
    s * (s - a) * (s - b) * (s - c);
  },
  }
};

fn classify(x: Int): String {
  match x {
  case 0 => "zero",
  case n if n > 0 and n < 10 => "single digit positive",
  case n if n < 0 and n > -10 => "single digit negative",
  case _ => "multi-digit",
  }
};

fn first[T](list: [T]): ?T {
  match list {
  case [] => None,
  case [head :: _] => Some(head),
  }
};

fn sum_tuple(pair: (Int, Int)): Int {
  val (a, b) := pair;
  a + b
};

record Person {
  name: String,
  age: Int,
};

fn greet_person(p: Person): String {
  val Person.{ name, age } := p;
  $"Hello {name}, you are {age} years old"
};
