/// An unbalanced binary search tree mapping keys to values.
///
/// `Map` is a persistent, ordered dictionary. Keys must implement `Ord`.
/// The empty map is `Empty`.
///
/// # Examples
///
/// ```musi
/// let m := insert(.Empty, "x", 1);
/// let m := insert(m, "y", 2);
/// get(m, "x")  -- .Some(1)
/// ```
export let Map ['K, 'V] := choice { Empty + Node of ('K, 'V, Map of ('K, 'V), Map of ('K, 'V)) };

/// Inserts a key-value pair, replacing any existing value for that key.
export let insert ['K, 'V] where 'K <: Ord : (Map of ('K, 'V), 'K, 'V) -> Map of ('K, 'V) :=
    (m, key, val) => match m (
        .Empty => .Node(key, val, .Empty, .Empty)
      | .Node(k, v, left, right) => match (compare(key, k)) (
            .Lt => .Node(k, v, insert(left, key, val), right)
          | .Eq => .Node(key, val, left, right)
          | .Gt => .Node(k, v, left, insert(right, key, val))
        )
    );

/// Returns the value associated with `key`, or `None` if not found.
export let get ['K, 'V] where 'K <: Ord : (Map of ('K, 'V), 'K) -> ?'V :=
    (m, key) => match m (
        .Empty => .None
      | .Node(k, v, left, right) => match (compare(key, k)) (
            .Lt => get(left, key)
          | .Eq => .Some(v)
          | .Gt => get(right, key)
        )
    );

/// Returns `True` if the map contains an entry for the given key.
export let contains_key ['K, 'V] where 'K <: Ord : (Map of ('K, 'V), 'K) -> Bool :=
    (m, key) => is_some(get(m, key));

/// Returns the number of entries in the map.
export let size ['K, 'V] : (Map of ('K, 'V)) -> Int :=
    (m) => match m (
        .Empty => 0
      | .Node(_, _, left, right) => 1 + size(left) + size(right)
    );

/// Returns `True` if the map has no entries.
export let is_empty ['K, 'V] : (Map of ('K, 'V)) -> Bool :=
    (m) => match m (.Empty => .True | _ => .False);

/// Collects all keys into a list in sorted order.
export let keys ['K, 'V] : (Map of ('K, 'V)) -> List of 'K :=
    (m) => match m (
        .Empty => .Nil
      | .Node(k, _, left, right) => append(keys(left), .Cons(k, keys(right)))
    );

/// Collects all values into a list in key-sorted order.
export let values ['K, 'V] : (Map of ('K, 'V)) -> List of 'V :=
    (m) => match m (
        .Empty => .Nil
      | .Node(_, v, left, right) => append(values(left), .Cons(v, values(right)))
    );

/// Collects all entries into a list of key-value pairs in sorted order.
export let to_list ['K, 'V] : (Map of ('K, 'V)) -> List of ('K, 'V) :=
    (m) => match m (
        .Empty => .Nil
      | .Node(k, v, left, right) => append(to_list(left), .Cons((k, v), to_list(right)))
    );
