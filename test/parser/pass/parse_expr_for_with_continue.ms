val process := fn () {
  val items := [1, 2, 3, 4, 5];
  for x in items do {
    if x mod 2 = 0 then {
      continue;
    };
    writeln(x);
  };
};
