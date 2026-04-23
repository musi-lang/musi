let Core := import "musi:core";
let Int := Core.Int;
let String := Core.String;

export opaque let Text := effect {
  let length (value : String) : Int;
  let concat (left : String, right : String) : String;
  let slice (value : String, start : Int, end : Int) : String;
  let byteAt (value : String, index : Int) : Int;
  let fromByte (value : Int) : String;
};

export let length (value : String) : Int := ask Text.length(value);

export let concat (left : String, right : String) : String :=
  ask Text.concat(left, right);

export let slice (value : String, start : Int, end : Int) : String :=
  ask Text.slice(value, start, end);

export let byteAt (value : String, index : Int) : Int :=
  ask Text.byteAt(value, index);

export let fromByte (value : Int) : String := ask Text.fromByte(value);
