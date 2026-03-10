/// An unbalanced binary search tree set.
export let Set := forall 'T -> Empty + Node of ('T, Set of 'T, Set of 'T);

/// Insert an element into the set.
export let insert : forall 'T where 'T <: Ord -> (Set of 'T, 'T) -> Set of 'T :=
    forall 'T where 'T <: Ord -> (s, value) -> match s (
        .Empty => .Node(value, .Empty, .Empty)
      | .Node(v, left, right) => match compare(value, v) (
            .Lt => .Node(v, insert(left, value), right)
          | .Eq => s
          | .Gt => .Node(v, left, insert(right, value))
        )
    );

/// True if the set contains the given element.
export let contains : forall 'T where 'T <: Ord -> (Set of 'T, 'T) -> Bool :=
    forall 'T where 'T <: Ord -> (s, value) -> match s (
        .Empty => .False
      | .Node(v, left, right) => match compare(value, v) (
            .Lt => contains(left, value)
          | .Eq => .True
          | .Gt => contains(right, value)
        )
    );

/// Return the number of elements.
export let size : forall 'T -> (Set of 'T) -> Int :=
    forall 'T -> (s) -> match s (
        .Empty => 0
      | .Node(_, left, right) => 1 + size(left) + size(right)
    );

/// True if the set is empty.
export let is_empty : forall 'T -> (Set of 'T) -> Bool :=
    forall 'T -> (s) -> match s (.Empty => .True | _ => .False);

/// Combine two sets.
export let union : forall 'T where 'T <: Ord -> (Set of 'T, Set of 'T) -> Set of 'T :=
    forall 'T where 'T <: Ord -> (a, b) -> match b (
        .Empty => a
      | .Node(v, left, right) => union(union(insert(a, v), left), right)
    );

/// Keep only elements present in both sets.
export let intersection : forall 'T where 'T <: Ord -> (Set of 'T, Set of 'T) -> Set of 'T :=
    forall 'T where 'T <: Ord -> (a, b) -> (
        let go(s, acc) := match s (
            .Empty => acc
          | .Node(v, left, right) =>
                go(right, go(left, (insert(acc, v) if contains(b, v) | acc if _)))
        );
        go(a, .Empty)
    );

/// Collect all elements into a sorted list.
export let to_list : forall 'T -> (Set of 'T) -> List of 'T :=
    forall 'T -> (s) -> match s (
        .Empty => .Nil
      | .Node(v, left, right) => append(to_list(left), .Cons(v, to_list(right)))
    );
