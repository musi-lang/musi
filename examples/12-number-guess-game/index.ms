let io := import "@std/io";
let math := import "@std/math";

export hidden let GuessResult := data {
  | TooLow(distance : Int)
  | Correct()
  | TooHigh(distance : Int)
};

export let target := 42;

export let scoreGuess (guess : Int) :=
  if guess < target then .TooLow(distance := math.abs(target - guess))
  else if guess > target then .TooHigh(distance := math.abs(guess - target))
  else .Correct();

export let hint (guess : Int) :=
  match scoreGuess(guess) (
  | .TooLow(distance := distance) => ("raise", distance)
  | .Correct() => ("win", 0)
  | .TooHigh(distance := distance) => ("lower", distance)
  );

export let demo () := (hint(12), hint(42), hint(80));

demo();
io.writeLn("game: guesses produced raise/win/lower hints");
