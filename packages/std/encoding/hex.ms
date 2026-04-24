let option := import "@std/option";
let both (left : Bool, right : Bool) : Bool :=
  match left (
  | ok if ok => right
  | _ => 0 = 1
  );

let text := import "@std/text";
let Option := option.Option;

let hexDigit (value : Int) : String :=
  match value (
  | 0 => "0"
  | 1 => "1"
  | 2 => "2"
  | 3 => "3"
  | 4 => "4"
  | 5 => "5"
  | 6 => "6"
  | 7 => "7"
  | 8 => "8"
  | 9 => "9"
  | 10 => "a"
  | 11 => "b"
  | 12 => "c"
  | 13 => "d"
  | 14 => "e"
  | _ => "f"
  );

let rec encodeFrom (source : String, index : Int, end : Int) : String :=
  match () (
  | _ if index >= end => ""
  | _ =>
  (
    let value := text.byteAt(source, index);
    text.concat(
      text.concat(hexDigit(value / 16), hexDigit(value % 16)),
      encodeFrom(source, index + 1, end),
    )
  )
);

--- Encode a value into this format.
export let encode (source : String) : String :=
  encodeFrom(source, 0, source.length());

let hexValue (value : Int) : Int :=
  match () (
  | _ if both(value >= 48, value <= 57) => value - 48
  | _ if both(value >= 65, value <= 70) => value - 55
  | _ if both(value >= 97, value <= 102) => value - 87
  | _ => - 1
  );

let rec isValidFrom (source : String, index : Int, end : Int) : Bool :=
  match () (
  | _ if index >= end => 0 = 0
  | _ if hexValue(text.byteAt(source, index)) < 0 => 0 = 1
  | _ => isValidFrom(source, index + 1, end)
  );

--- Returns true when source is valid for this format.
export let isValid (source : String) : Bool :=
  match () (
  | _ if source.length() % 2 /= 0 => 0 = 1
  | _ => isValidFrom(source, 0, source.length())
  );

let rec decodeFrom (source : String, index : Int, end : Int) : String :=
  match () (
  | _ if index >= end => ""
  | _ =>
  (
    let hi := hexValue(text.byteAt(source, index));
    let lo := hexValue(text.byteAt(source, index + 1));
    text.concat(text.fromByte(hi * 16 + lo), decodeFrom(source, index + 2, end))
  )
);

--- Decode a value from this format.
export let decode (source : String) : Option[String] :=
  match isValid(source) (
  | valid if valid => option.someOf[String](
    decodeFrom(source, 0, source.length())
  )
  | _ => option.noneOf[String]()
  );
