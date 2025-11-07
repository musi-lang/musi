import { Nat } from "stdlib/lang/Nat";

const Text := record {
  var _data: Any;
};

/// Returns length of given text.
const Text.length := proc (_s: Text) -> Nat {
  return 0;
};

export { Text };
