let cryptoHost := import "musi:crypto";
let randomHost := import "musi:random";

--- Secure random bytes encoded as hex text.
export let randomBytes (count : Int) : String := randomHost.entropyHex(count);

--- SHA-256 digest encoded as hex text.
export let sha256Hex (source : String) : String := cryptoHost.sha256Hex(source);

--- SHA-256 digest encoded as base64 text.
export let sha256Base64 (source : String) : String :=
  cryptoHost.sha256Base64(source);
