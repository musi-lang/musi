let option := import "@std/option";

export let defaultGate : option.Option[Int] := option.someOf[Int](1);
