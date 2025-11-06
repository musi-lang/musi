const sum := proc (items: [Int]) -> Int {
  var total := 0;
  for x in items do {
    total <- total + x;
  };
  return total;
};

const find := proc (items: [Int], target: Int) -> Int {
  for x in items do {
    if x = target then {
      break x;
    };
  };
  return -1;
};

const numbers := [1, 2, 3, 4, 5];
const result := sum(numbers);
const found := find(numbers, 3);
