import { Nat } from "stdlib/lang/Nat";

@link("musi.lang.Text")
const Text := record {
  var _data: Any;
};

@link("musi.lang.Text.length")
/// Returns length of given text.
const Text.length := proc (_s: Text) -> Nat {
  return 0;
};

export { Text };
