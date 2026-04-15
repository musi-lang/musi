let OptionModule := import "@std/option";
let Runtime := import "musi:runtime";
let Option := OptionModule.Option;

export let encode (source : String) : String := Runtime.hexEncode(source);

export let isValid (source : String) : Bool := Runtime.hexIsValid(source) > 0;

export let decode (source : String) : Option[String] :=
  match isValid(source) (
  | valid if valid => OptionModule.some[String](Runtime.hexDecode(source))
  | _ => OptionModule.none[String]()
  );
