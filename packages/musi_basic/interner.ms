import { create, push, get, set, length } from "stdlib/collections/array";
import { equals } from "stdlib/text/text";

const BASE_ALLOC_SIZE := 256;

export const Interner := record {
  strings: [Text],
  count: Nat,
  table: [Text]  // Placeholder for now - simple linear search
};

export const create := proc () -> Interner {
  Interner{
    .strings := create(BASE_ALLOC_SIZE),
    .count := 0,
    .table := create(BASE_ALLOC_SIZE)
  }
};

export const intern := proc (interner: Interner, str: Text) -> name {
  // Simple linear search for now
  const i := 0;
  while i < interner.count do {
    const existing := get(interner.strings, i);
    if equals(existing, str) then {
      return i
    };
    i <- i + 1
  };

  // Not found, add new string
  const name := interner.count;
  set(interner.strings, name, str);
  set(interner.table, name, str);
  interner.count <- interner.count + 1;
  name
};

export const lookup := proc (interner: Interner, name: name) -> Text {
  if name >= 0 and then name < interner.count then {
    get(interner.strings, name)
  } else {
    ""
  }
};

export const equal := proc (a: name, b: name) -> Bool {
  a = b
};

export const empty_name := proc (interner: Interner) -> name {
  intern(interner, "")
};
