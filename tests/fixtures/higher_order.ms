/// Higher-order functions and closures
/// Tests function values, pipes, and lambda semantics

fn map[T, U](list: [T], f: T -> U): [U] {
  var result := [];
  for item in list {
    result <- result :: f(item);
  };
  result
};

fn filter[T](list: [T], pred: T -> Bool): [T] {
  var result := [];
  for item in list {
    if pred(item) {
      result <- result :: item;
    };
  };
  result
};

fn fold[T, U](list: [T], init: U, f: (U, T) -> U): U {
  var acc := init;
  for item in list {
    acc <- f(acc, item);
  };
  acc
};

val numbers := [1, 2, 3, 4, 5];

val doubled := numbers |> map(fn (x) => x * 2);
val evens := numbers |> filter(fn (x) => x % 2 = 0);
val total := numbers |> fold(0, fn (acc, x) => acc + x);

fn compose[A, B, C](f: A -> B, g: B -> C): A -> C {
  fn (x) => g(f(x))
};

val add_one := fn (x: Int) => x + 1;
val double := fn (x: Int) => x * 2;
val add_one_then_double := compose(add_one, double);

val piped_result := 5
  |> add_one
  |> double
  |> fn (x) => x - 1;
