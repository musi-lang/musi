--! Semantic version helpers.

let text := import "@std/text";

--- Semantic version value.
export hidden let Version := data {
  | VersionText(value : String)
};

--- Parse semantic version text.
export let parse (source : String) := .VersionText(value := source);

let both (left : Bit, right : Bit) := if left then right else 0 = 1;

let isDigit (value : Int) := both(value >= 48, value <= 57);

let isDigitAt (source : String, index : Int) :=
  isDigit(text.byteAt(source, index));

--- Returns true when text is a valid semantic version.
export let isValid (source : String) :=
  both(
    both(source.length() >= 5, isDigitAt(source, 0)),
    both(
      both(text.byteAt(source, 1) = 46, isDigitAt(source, 2)),
      both(text.byteAt(source, 3) = 46, isDigitAt(source, 4)),
    ),
  );

let byteOrZero (source : String, index : Int) :=
  if text.byteAt(source, index) < 0 then 0 else text.byteAt(source, index);

let compareByte (left : String, right : String, index : Int) :=
  (
    let diff := byteOrZero(left, index) - byteOrZero(right, index);
    if diff < 0 then -1 else if diff > 0 then 1 else 0;
  );

--- Compare semantic versions.
export let compare (left : Version, right : Version) :=
  match left (
  | .VersionText(value := leftText) => match right (
  | .VersionText(value := rightText) =>
  (
    let major := compareByte(leftText, rightText, 0);
    if major /= 0 then major
    else
      (
        let minor := compareByte(leftText, rightText, 2);
        if minor /= 0 then minor else compareByte(leftText, rightText, 4)
      )
  )
)
);

--- Return normalized semantic version text.
export let (self : Version).toString () :=
  match self (
  | .VersionText(value := value) => value
  );
