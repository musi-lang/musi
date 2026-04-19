let option := import "@std/option";

let findGate (ticket : String) : option.Option[Int] :=
  match ticket (
  | "A12" => option.someOf[Int](4)
  | _ => option.noneOf[Int]()
  );

findGate("A12");
