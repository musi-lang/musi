/// Return a random float in the range [0, 1).
export let random : () ~> Float64 under { State };

/// Return a random integer in the range [lo, hi].
export let random_int : (Int, Int) ~> Int under { State };

/// Return a random boolean.
export let random_bool : () ~> Bool under { State };

/// Choose a random element from a list, or `None` if empty.
export let choose ['T] : (List of 'T) ~> ?'T under { State };
