/// An unbalanced binary search tree set.
///
/// `Set` is a persistent, ordered collection of unique elements.
/// Elements must implement `Ord`. The empty set is `Empty`.
///
/// # Examples
///
/// ```musi
/// let s := insert(.Empty, 3);
/// let s := insert(s, 1);
/// contains(s, 3)  // .True
/// ```
export let Set ['T] := choice { Empty + Node of ('T, Set of 'T, Set of 'T) };

/// Inserts an element into the set, returning the set unchanged if already present.
export let insert ['T] where 'T <: Ord : (Set of 'T, 'T) -> Set of 'T :=
    (s, value) => match s (
        .Empty => .Node(value, .Empty, .Empty)
      | .Node(v, left, right) => match (compare(value, v)) (
            .Lt => .Node(v, insert(left, value), right)
          | .Eq => s
          | .Gt => .Node(v, left, insert(right, value))
        )
    );

/// Returns `True` if the set contains the given element.
export let contains ['T] where 'T <: Ord : (Set of 'T, 'T) -> Bool :=
    (s, value) => match s (
        .Empty => .False
      | .Node(v, left, right) => match (compare(value, v)) (
            .Lt => contains(left, value)
          | .Eq => .True
          | .Gt => contains(right, value)
        )
    );

/// Returns the number of elements in the set.
export let size ['T] : (Set of 'T) -> Int :=
    (s) => match s (
        .Empty => 0
      | .Node(_, left, right) => 1 + size(left) + size(right)
    );

/// Returns `True` if the set has no elements.
export let is_empty ['T] : (Set of 'T) -> Bool :=
    (s) => match s (.Empty => .True | _ => .False);

/// Returns the union of two sets, containing all elements from both.
///
/// # Examples
///
/// ```musi
/// let a := insert(insert(.Empty, 1), 2);
/// let b := insert(insert(.Empty, 2), 3);
/// to_list(union(a, b))  // [1, 2, 3]
/// ```
export let union ['T] where 'T <: Ord : (Set of 'T, Set of 'T) -> Set of 'T :=
    (a, b) => match b (
        .Empty => a
      | .Node(v, left, right) => union(union(insert(a, v), left), right)
    );

/// Returns the intersection of two sets, keeping only elements present in both.
export let intersection ['T] where 'T <: Ord : (Set of 'T, Set of 'T) -> Set of 'T :=
    (a, b) => (
        let go(s, acc) := match s (
            .Empty => acc
          | .Node(v, left, right) =>
                go(right, go(left, (insert(acc, v) if contains(b, v) | acc if _)))
        );
        go(a, .Empty)
    );

/// Collects all elements into a sorted list via in-order traversal.
export let to_list ['T] : (Set of 'T) -> List of 'T :=
    (s) => match s (
        .Empty => .Nil
      | .Node(v, left, right) => append(to_list(left), .Cons(v, to_list(right)))
    );
