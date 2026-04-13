let OptionModule := import "@std/option";
let Option := OptionModule.Option;

export let encode (source : String) : String := source;

export let isValid (source : String) : Bool := 0 = 0;

export let decode (source : String) : Option[String] :=
  case isValid(source) of (
  | valid if valid => OptionModule.some[String](source)
  | _ => OptionModule.none[String]()
  );
