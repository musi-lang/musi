const classify := proc (x: Int) -> Text {
  match x with {
  case 0 -> "zero",
  case const n if n > 0 -> "positive",
  case const n if n < 0 -> "negative",
  case _ -> "unknown",
  }
};
