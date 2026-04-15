let OptionModule := import "@std/option";
let Runtime := import "musi:runtime";
let Option := OptionModule.Option;

export let encode (source : String) : String := Runtime.utf8Encode(source);

export let isValid (bytes : String) : Bool := Runtime.utf8IsValid(bytes) > 0;

export let decode (bytes : String) : Option[String] :=
  match isValid(bytes) (
  | valid if valid => OptionModule.some[String](Runtime.utf8Decode(bytes))
  | _ => OptionModule.none[String]()
  );
