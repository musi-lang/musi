let OptionModule := import "@std/option";
let Option := OptionModule.Option;

export let encode (source : String) : String := source;

export let isValid (source : String) : Bool := 0 = 0;

export let decode (source : String) : Option[String] :=
  match isValid(source) (
  | valid if valid => OptionModule.some[String](source)
  | _ => OptionModule.none[String]()
  );
