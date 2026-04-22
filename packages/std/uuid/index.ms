let text := import "@std/text";
let uuidHost := import "musi:uuid";

--- UUID text value.
export opaque let Uuid := data {
  | Uuid(value : String)
};

--- Generate a random version-4 UUID.
export let v4 () : Uuid := .Uuid(value := uuidHost.v4());

--- Parse UUID text.
export let parse (source : String) : Uuid := .Uuid(value := source);

let both (left : Bool, right : Bool) : Bool :=
  match left (
  | ok if ok => right
  | _ => 0 = 1
  );

let any3 (first : Bool, second : Bool, third : Bool) : Bool :=
  match first (
  | ok if ok => 0 = 0
  | _ =>
    match second (
    | ok if ok => 0 = 0
    | _ => third
    )
  );

let isHyphenAt (source : String, index : Int) : Bool :=
  text.byteAt(source, index) = 45;

let inRange (value : Int, lower : Int, upper : Int) : Bool :=
  both(value >= lower, value <= upper);

let isHexByte (value : Int) : Bool :=
  any3(inRange(value, 48, 57), inRange(value, 65, 70), inRange(value, 97, 102));

let isUuidHexAt (source : String, index : Int) : Bool :=
  isHexByte(text.byteAt(source, index));

let isUuidPrefixValid (source : String) : Bool :=
  both(
    both(
      both(isUuidHexAt(source, 0), isUuidHexAt(source, 1)),
      both(isUuidHexAt(source, 2), isUuidHexAt(source, 3)),
    ),
    both(
      both(isUuidHexAt(source, 4), isUuidHexAt(source, 5)),
      both(isUuidHexAt(source, 6), isUuidHexAt(source, 7)),
    ),
  );

let isUuidMiddleValid (source : String) : Bool :=
  both(
    both(
      both(isUuidHexAt(source, 9), isUuidHexAt(source, 10)),
      both(isUuidHexAt(source, 11), isUuidHexAt(source, 12)),
    ),
    both(
      both(isUuidHexAt(source, 14), isUuidHexAt(source, 15)),
      both(isUuidHexAt(source, 16), isUuidHexAt(source, 17)),
    ),
  );

let isUuidTailValid (source : String) : Bool :=
  both(
    both(
      both(isUuidHexAt(source, 19), isUuidHexAt(source, 20)),
      both(isUuidHexAt(source, 21), isUuidHexAt(source, 22)),
    ),
    both(
      both(isUuidHexAt(source, 24), isUuidHexAt(source, 25)),
      both(isUuidHexAt(source, 26), isUuidHexAt(source, 27)),
    ),
  );

let isUuidLastValid (source : String) : Bool :=
  both(
    both(
      both(isUuidHexAt(source, 28), isUuidHexAt(source, 29)),
      both(isUuidHexAt(source, 30), isUuidHexAt(source, 31)),
    ),
    both(
      both(isUuidHexAt(source, 32), isUuidHexAt(source, 33)),
      both(isUuidHexAt(source, 34), isUuidHexAt(source, 35)),
    ),
  );

let hasUuidHyphens (source : String) : Bool :=
  both(
    both(isHyphenAt(source, 8), isHyphenAt(source, 13)),
    both(isHyphenAt(source, 18), isHyphenAt(source, 23)),
  );

--- Returns true when text is a valid UUID.
export let isValid (source : String) : Bool :=
  both(
    both(source.length() = 36, hasUuidHyphens(source)),
    both(
      both(isUuidPrefixValid(source), isUuidMiddleValid(source)),
      both(isUuidTailValid(source), isUuidLastValid(source)),
    ),
  );

--- Return canonical UUID text.
export let(self : Uuid).toString () : String :=
  match self (
  | .Uuid(value := value) => value
  );
