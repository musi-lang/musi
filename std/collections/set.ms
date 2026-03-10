/// An unbalanced binary search tree set.
export let Set ['T] := choice { Empty + Node of ('T, Set of 'T, Set of 'T) };

/// Insert an element into the set.
export let insert ['T] where 'T <: Ord : (Set of 'T, 'T) -> Set of 'T :=
    (s, value) => match s (
        .Empty => .Node(value, .Empty, .Empty)
      | .Node(v, left, right) => match (compare(value, v)) (
            .Lt => .Node(v, insert(left, value), right)
          | .Eq => s
          | .Gt => .Node(v, left, insert(right, value))
        )
    );

/// True if the set contains the given element.
export let contains ['T] where 'T <: Ord : (Set of 'T, 'T) -> Bool :=
    (s, value) => match s (
        .Empty => .False
      | .Node(v, left, right) => match (compare(value, v)) (
            .Lt => contains(left, value)
          | .Eq => .True
          | .Gt => contains(right, value)
        )
    );

/// Return the number of elements.
export let size ['T] : (Set of 'T) -> Int :=
    (s) => match s (
        .Empty => 0
      | .Node(_, left, right) => 1 + size(left) + size(right)
    );

/// True if the set is empty.
export let is_empty ['T] : (Set of 'T) -> Bool :=
    (s) => match s (.Empty => .True | _ => .False);

/// Combine two sets.
export let union ['T] where 'T <: Ord : (Set of 'T, Set of 'T) -> Set of 'T :=
    (a, b) => match b (
        .Empty => a
      | .Node(v, left, right) => union(union(insert(a, v), left), right)
    );

/// Keep only elements present in both sets.
export let intersection ['T] where 'T <: Ord : (Set of 'T, Set of 'T) -> Set of 'T :=
    (a, b) => (
        let go(s, acc) := match s (
            .Empty => acc
          | .Node(v, left, right) =>
                go(right, go(left, (insert(acc, v) if contains(b, v) | acc if _)))
        );
        go(a, .Empty)
    );

/// Collect all elements into a sorted list.
export let to_list ['T] : (Set of 'T) -> List of 'T :=
    (s) => match s (
        .Empty => .Nil
      | .Node(v, left, right) => append(to_list(left), .Cons(v, to_list(right)))
    );
