const classify := proc (x: Int) -> Text {
  match x with {
  case 0 -> "zero",
  case 1 -> "one",
  case const n if n > 0 -> "positive",
  case const n if n < 0 -> "negative",
  case _ -> "unknown",
  }
};

const result := classify(5);
const status := classify(-3);
const zero := classify(0);
