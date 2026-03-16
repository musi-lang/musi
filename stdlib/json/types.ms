//! JSON value types.

export let JsonValue := choice {
    Null
  + Bool of Bool
  + Number of Float64
  + Str of String
  + Array of []JsonValue
  + Object of [](String, JsonValue)
};
