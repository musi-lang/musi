val find_first := fn (items: [Int], target: Int) -> Int {
  for x in items do {
    if x = target then {
      break x;
    };
  };
  -1
};

val skip_evens := fn (items: [Int]) {
  for x in items do {
    if x mod 2 = 0 then {
      continue;
    };
    writeln(x);
  };
};

val result := find_first([1, 2, 3, 4, 5], 3);
skip_evens([1, 2, 3, 4, 5, 6]);
