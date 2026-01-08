/// Control flow tests
/// Tests if/else, match, for, while semantic handling

fn abs(x: Int): Int {
  if x < 0 { -x } else { x }
};

fn sign(x: Int): Int {
  if x > 0 { 1 }
  else if x < 0 { -1 }
  else { 0 }
};

fn factorial(n: Int): Int {
  if n <= 1 { 1 }
  else { n * factorial(n - 1) }
};

fn describe(x: Int): String {
  match x {
  case 0 => "zero",
  case 1 => "one",
  case n if n < 0 => "negative",
  case _ => "many",
  }
};

fn sum_to(n: Int): Int {
  var total := 0;
  for i in 1..n {
    total <- total + i;
  };
  total
};

fn countdown(n: Int): Int {
  var count := n;
  while count > 0 {
    count <- count - 1;
  };
  count
};
