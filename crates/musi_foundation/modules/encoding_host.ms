let Core := import "musi:core";
let Int := Core.Int;
let String := Core.String;

export opaque let EncodingHost := effect {
  let base64Encode (source : String) : String;
  let base64Decode (source : String) : String;
  let base64IsValid (source : String) : Int;
  let utf8Encode (source : String) : String;
  let utf8Decode (source : String) : String;
  let utf8IsValid (source : String) : Int;
};

export let base64Encode (source : String) : String :=
  ask EncodingHost.base64Encode(source);

export let base64Decode (source : String) : String :=
  ask EncodingHost.base64Decode(source);

export let base64IsValid (source : String) : Int :=
  ask EncodingHost.base64IsValid(source);

export let utf8Encode (source : String) : String :=
  ask EncodingHost.utf8Encode(source);

export let utf8Decode (source : String) : String :=
  ask EncodingHost.utf8Decode(source);

export let utf8IsValid (source : String) : Int :=
  ask EncodingHost.utf8IsValid(source);
