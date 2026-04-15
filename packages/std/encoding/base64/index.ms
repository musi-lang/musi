let OptionModule := import "@std/option";
let Runtime := import "musi:runtime";
let Option := OptionModule.Option;

export let encode (source : String) : String := Runtime.base64Encode(source);

export let isValid (source : String) : Bool := Runtime.base64IsValid(source) > 0;

export let decode (source : String) : Option[String] :=
  match isValid(source) (
  | valid if valid => OptionModule.some[String](Runtime.base64Decode(source))
  | _ => OptionModule.none[String]()
  );
