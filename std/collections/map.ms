/// An unbalanced binary search tree mapping keys to values.
export let Map := forall 'K, 'V -> Empty + Node of ('K, 'V, Map of ('K, 'V), Map of ('K, 'V));

/// Insert a key-value pair, replacing any existing value for the key.
export let insert : forall 'K, 'V where 'K <: Ord -> (Map of ('K, 'V), 'K, 'V) -> Map of ('K, 'V) :=
    forall 'K, 'V where 'K <: Ord -> (m, key, val) -> match m (
        .Empty => .Node(key, val, .Empty, .Empty)
      | .Node(k, v, left, right) => match compare(key, k) (
            .Lt => .Node(k, v, insert(left, key, val), right)
          | .Eq => .Node(key, val, left, right)
          | .Gt => .Node(k, v, left, insert(right, key, val))
        )
    );

/// Look up a value by key.
export let get : forall 'K, 'V where 'K <: Ord -> (Map of ('K, 'V), 'K) -> ?'V :=
    forall 'K, 'V where 'K <: Ord -> (m, key) -> match m (
        .Empty => .None
      | .Node(k, v, left, right) => match compare(key, k) (
            .Lt => get(left, key)
          | .Eq => .Some(v)
          | .Gt => get(right, key)
        )
    );

/// True if the map contains the given key.
export let contains_key : forall 'K, 'V where 'K <: Ord -> (Map of ('K, 'V), 'K) -> Bool :=
    forall 'K, 'V where 'K <: Ord -> (m, key) -> is_some(get(m, key));

/// Return the number of entries.
export let size : forall 'K, 'V -> (Map of ('K, 'V)) -> Int :=
    forall 'K, 'V -> (m) -> match m (
        .Empty => 0
      | .Node(_, _, left, right) => 1 + size(left) + size(right)
    );

/// True if the map is empty.
export let is_empty : forall 'K, 'V -> (Map of ('K, 'V)) -> Bool :=
    forall 'K, 'V -> (m) -> match m (.Empty => .True | _ => .False);

/// Collect all keys into a list (in-order).
export let keys : forall 'K, 'V -> (Map of ('K, 'V)) -> List of 'K :=
    forall 'K, 'V -> (m) -> match m (
        .Empty => .Nil
      | .Node(k, _, left, right) => append(keys(left), .Cons(k, keys(right)))
    );

/// Collect all values into a list (in key order).
export let values : forall 'K, 'V -> (Map of ('K, 'V)) -> List of 'V :=
    forall 'K, 'V -> (m) -> match m (
        .Empty => .Nil
      | .Node(_, v, left, right) => append(values(left), .Cons(v, values(right)))
    );

/// Collect all entries into a list of pairs (in key order).
export let to_list : forall 'K, 'V -> (Map of ('K, 'V)) -> List of ('K, 'V) :=
    forall 'K, 'V -> (m) -> match m (
        .Empty => .Nil
      | .Node(k, v, left, right) => append(to_list(left), .Cons((k, v), to_list(right)))
    );
