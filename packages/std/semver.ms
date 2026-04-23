let text := import "@std/text";

--- Semantic version value.
export opaque let Version := data {
  | VersionText(value : String)
};

--- Parse semantic version text.
export let parse (source : String) : Version := .VersionText(value := source);

let both (left : Bool, right : Bool) : Bool :=
  match left (
  | ok if ok => right
  | _ => 0 = 1
  );

let isDigit (value : Int) : Bool := both(value >= 48, value <= 57);

let isDigitAt (source : String, index : Int) : Bool :=
  isDigit(text.byteAt(source, index));

--- Returns true when text is a valid semantic version.
export let isValid (source : String) : Bool :=
  both(
    both(source.length() >= 5, isDigitAt(source, 0)),
    both(
      both(text.byteAt(source, 1) = 46, isDigitAt(source, 2)),
      both(text.byteAt(source, 3) = 46, isDigitAt(source, 4)),
    ),
  );

let byteOrZero (source : String, index : Int) : Int :=
  match text.byteAt(source, index) (
  | value if value < 0 => 0
  | value => value
  );

let compareByte (left : String, right : String, index : Int) : Int :=
  match byteOrZero(left, index) - byteOrZero(right, index) (
  | diff if diff < 0 => - 1
  | diff if diff > 0 => 1
  | _ => 0
  );

--- Compare semantic versions.
export let compare (left : Version, right : Version) : Int :=
  match left (
  | .VersionText(value := leftText) => match right (
  | .VersionText(value := rightText) =>
  (
    let major := compareByte(leftText, rightText, 0);
    match major (
    | result if result /= 0 => result
    | _ =>
      (
        let minor := compareByte(leftText, rightText, 2);
        match minor (
        | result if result /= 0 => result
        | _ => compareByte(leftText, rightText, 4)
        )
      )
    )
  )
)
);

--- Return normalized semantic version text.
export let (self : Version).toString () : String :=
  match self (
  | .VersionText(value := value) => value
  );
