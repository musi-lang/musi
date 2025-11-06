const find_first := proc (items: [Int], target: Int) -> Int {
  for x in items do {
    if x = target then {
      break x;
    };
  };
  return -1;
};

const skip_evens := proc (items: [Int]) {
  for x in items do {
    if x mod 2 = 0 then {
      continue;
    };
    writeln(x);
  };
};

const result := find_first([1, 2, 3, 4, 5], 3);
skip_evens([1, 2, 3, 4, 5, 6]);
