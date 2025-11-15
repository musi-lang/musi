val classify := fn (x: Int) -> Str {
  match x with {
  case 0 -> "zero",
  case n if n > 0 -> "positive",
  case n if n < 0 -> "negative",
  case _ -> "unknown",
  }
};
