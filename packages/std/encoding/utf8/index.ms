let OptionModule := import "@std/option";
let Option := OptionModule.Option;

export let encode (source : String) : String := source;

export let isValid (bytes : String) : Bool := 0 = 0;

export let decode (bytes : String) : Option[String] :=
  case isValid(bytes) of (
  | valid if valid => OptionModule.some[String](bytes)
  | _ => OptionModule.none[String]()
  );
