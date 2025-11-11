// again, we put choice type here. since we have no nullptrs etc, optionals are the closest we have to "undefined" values
// so we can use Option<T> to represent values that may or may not be present
export const Option := choice Option<T> {
  case Some(T),
  case None,
};

// convenience procs for checking presence of value (are we doing properly yet?)
export const Option.is_some := proc (opt: Option<T>) -> Bool {
  if opt = .Some(_) then true else false
};

export const Option.is_none := proc (opt: Option<T>) -> Bool {
  if opt = .None then true else false
};
