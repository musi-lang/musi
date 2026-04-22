let Core := import "musi:core";
let Int := Core.Int;
let String := Core.String;

export opaque let Text := effect {
  let length (value : String) : Int;
  let trim (value : String) : String;
  let concat (left : String, right : String) : String;
  let toLowerCase (value : String) : String;
  let toUpperCase (value : String) : String;
  let contains (value : String, needle : String) : Int;
  let startsWith (value : String, prefix : String) : Int;
  let endsWith (value : String, suffix : String) : Int;
  let indexOf (value : String, needle : String) : Int;
  let slice (value : String, start : Int, end : Int) : String;
  let byteAt (value : String, index : Int) : Int;
};

export let length (value : String) : Int := ask Text.length(value);
export let trim (value : String) : String := ask Text.trim(value);
export let concat (left : String, right : String) : String := ask Text.concat(left, right);
export let toLowerCase (value : String) : String := ask Text.toLowerCase(value);
export let toUpperCase (value : String) : String := ask Text.toUpperCase(value);
export let contains (value : String, needle : String) : Int := ask Text.contains(value, needle);
export let startsWith (value : String, prefix : String) : Int := ask Text.startsWith(value, prefix);
export let endsWith (value : String, suffix : String) : Int := ask Text.endsWith(value, suffix);
export let indexOf (value : String, needle : String) : Int := ask Text.indexOf(value, needle);
export let slice (value : String, start : Int, end : Int) : String := ask Text.slice(value, start, end);
export let byteAt (value : String, index : Int) : Int := ask Text.byteAt(value, index);
