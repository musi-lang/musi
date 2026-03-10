/// An unbalanced binary search tree mapping keys to values.
export let Map ['K, 'V] := choice { Empty + Node of ('K, 'V, Map of ('K, 'V), Map of ('K, 'V)) };

/// Insert a key-value pair, replacing any existing value for the key.
export let insert ['K, 'V] where 'K <: Ord : (Map of ('K, 'V), 'K, 'V) -> Map of ('K, 'V) :=
    (m, key, val) => match m (
        .Empty => .Node(key, val, .Empty, .Empty)
      | .Node(k, v, left, right) => match (compare(key, k)) (
            .Lt => .Node(k, v, insert(left, key, val), right)
          | .Eq => .Node(key, val, left, right)
          | .Gt => .Node(k, v, left, insert(right, key, val))
        )
    );

/// Look up a value by key.
export let get ['K, 'V] where 'K <: Ord : (Map of ('K, 'V), 'K) -> ?'V :=
    (m, key) => match m (
        .Empty => .None
      | .Node(k, v, left, right) => match (compare(key, k)) (
            .Lt => get(left, key)
          | .Eq => .Some(v)
          | .Gt => get(right, key)
        )
    );

/// True if the map contains the given key.
export let contains_key ['K, 'V] where 'K <: Ord : (Map of ('K, 'V), 'K) -> Bool :=
    (m, key) => is_some(get(m, key));

/// Return the number of entries.
export let size ['K, 'V] : (Map of ('K, 'V)) -> Int :=
    (m) => match m (
        .Empty => 0
      | .Node(_, _, left, right) => 1 + size(left) + size(right)
    );

/// True if the map is empty.
export let is_empty ['K, 'V] : (Map of ('K, 'V)) -> Bool :=
    (m) => match m (.Empty => .True | _ => .False);

/// Collect all keys into a list (in-order).
export let keys ['K, 'V] : (Map of ('K, 'V)) -> List of 'K :=
    (m) => match m (
        .Empty => .Nil
      | .Node(k, _, left, right) => append(keys(left), .Cons(k, keys(right)))
    );

/// Collect all values into a list (in key order).
export let values ['K, 'V] : (Map of ('K, 'V)) -> List of 'V :=
    (m) => match m (
        .Empty => .Nil
      | .Node(_, v, left, right) => append(values(left), .Cons(v, values(right)))
    );

/// Collect all entries into a list of pairs (in key order).
export let to_list ['K, 'V] : (Map of ('K, 'V)) -> List of ('K, 'V) :=
    (m) => match m (
        .Empty => .Nil
      | .Node(k, v, left, right) => append(to_list(left), .Cons((k, v), to_list(right)))
    );
