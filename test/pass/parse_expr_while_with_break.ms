const find := proc (target: Int) -> Int {
  var i := 0;
  while i < 100 do {
    if i = target then {
      break i;
    };
    i <- i + 1;
  };
  return -1;
};
