/// Type definitions test
/// Tests record, choice, and type alias semantic tokens

record Point {
  x: Int,
  y: Int,
};

record Point3D {
  x: Int,
  y: Int,
  z: Int,
};

choice Option[T] {
  case Some(T),
  case None,
};

choice Result[T, E] {
  case Ok(T),
  case Err(E),
};

type IntOption := Option[Int];
type StringResult := Result[String, String];

val origin := Point.{ x := 0, y := 0 };
val some_value := Some(42);
val none_value := None;

fn map_option[T, U](opt: Option[T], f: T -> U): Option[U] {
  match opt {
    case Some(x) => Some(f(x)),
    case None => None,
  };
};
