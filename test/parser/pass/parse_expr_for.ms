val sum := fn (items: [Int]) -> Int {
  var total := 0;
  for x in items do {
    total <- total + x;
  };
  total
};

val find := fn (items: [Int], target: Int) -> Int {
  for x in items do {
    if x = target then {
      break x;
    };
  };
  -1
};

val numbers := [1, 2, 3, 4, 5];
val result := sum(numbers);
val found := find(numbers, 3);
