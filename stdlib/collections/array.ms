const __builtin_array_new := unsafe extern "C" proc (capacity: Nat) -> [T];
const __builtin_array_push := unsafe extern "C" proc (arr: [T], item: T) -> ();
const __builtin_array_get := unsafe extern "C" proc (arr: [T], idx: Nat) -> T;
const __builtin_array_set := unsafe extern "C" proc (arr: [T], idx: Nat, item: T) -> ();
const __builtin_array_len := unsafe extern "C" proc (arr: [T]) -> Nat;

export const create := proc<T>(capacity: Nat) -> [T] {
  __builtin_array_new(capacity)
};

export const length := proc<T>(arr: [T]) -> Nat {
  __builtin_array_len(arr)
};

export const is_empty := proc<T>(arr: [T]) -> Bool {
  length(arr) = 0
};

export const push := proc<T>(arr: [T], item: T) -> () {
  __builtin_array_push(arr, item)
};

export const get := proc<T>(arr: [T], idx: Nat) -> T {
  __builtin_array_get(arr, idx)
};

export const set := proc<T>(arr: [T], idx: Nat, item: T) -> () {
  __builtin_array_set(arr, idx, item)
};
