fn safe_div(a: Int, b: Int): Option[Int] =>
    if b = 0 then None else Some(a / b);

match safe_div(10, 0) with (
    None    => writeln("division by zero")
  | Some(v) => writeln(int_to_string(v))
);

match safe_div(10, 2) with (
    None    => writeln("none")
  | Some(v) => writeln(int_to_string(v))
);

match safe_div(7, 3) with (
    None    => writeln("none")
  | Some(v) => writeln(int_to_string(v))
);
