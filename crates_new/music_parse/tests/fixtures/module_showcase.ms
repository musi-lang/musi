import "std/io" as IO.{read, write};
export opaque let id (x : T) [T] := x;
let Maybe := data { | Some : Int | None };
let Console := effect {
  let read() : Int;
  law pure(x) := x
};
let Show := class where T : Display {
  let render(x : T) : Int;
  law id(x) := x
};
instance [T] where T : Display Show[T] {
  let render(x : Show[T]) : Int := x;
  law id(x) := x
};
let run with { Read[Int], Write } := work;
