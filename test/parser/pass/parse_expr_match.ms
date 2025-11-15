val classify := fn (x: Int) -> Str {
  match x with {
  case 0 -> "zero",
  case 1 -> "one",
  case n if n > 0 -> "positive",
  case n if n < 0 -> "negative",
  case _ -> "unknown",
  }
};

val result := classify(5);
val status := classify(-3);
val zero := classify(0);
