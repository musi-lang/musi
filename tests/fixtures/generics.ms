/// Generic types and constraints
/// Tests type parameters and generic functions

fn swap[T](pair: (T, T)): (T, T) {
  val (a, b) := pair;
  (b, a)
};

fn repeat[T](value: T, count: Int): []T {
  var result := [];
  for _ in 0..<count {
    result <- result :: value;
  };
  result
};

record Pair[A, B] {
  first: A,
  second: B,
};

fn make_pair[A, B](a: A, b: B): Pair[A, B] {
  Pair.{ first := a, second := b };
};

choice Either[L, R] {
  case Left(L),
  case Right(R),
};

fn map_either[L, R, R2](e: Either[L, R], f: R -> R2): Either[L, R2] {
  match e {
  case Left(l) => Left(l),
  case Right(r) => Right(f(r)),
  }
};

fn flatten[T](nested: [][]T): []T {
  var result := [];
  for inner in nested {
    for item in inner {
      result <- result :: item;
    };
  };
  result
};

val int_pair := make_pair(1, 2);
val mixed_pair := make_pair("hello", true);

val swapped := swap((1, 2));
val threes := repeat(3, 5);
