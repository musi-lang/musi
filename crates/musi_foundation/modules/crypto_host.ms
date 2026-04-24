let Core := import "musi:core";
let String := Core.String;

export opaque let CryptoHost := effect {
  let sha256Hex (source : String) : String;
  let sha256Base64 (source : String) : String;
};

export let sha256Hex (source : String) : String :=
  ask CryptoHost.sha256Hex(source);

export let sha256Base64 (source : String) : String :=
  ask CryptoHost.sha256Base64(source);
