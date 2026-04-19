let Show [T] := class {
  let show (value : T) : String;
};

export instance Show[Int] {
  let show (value : Int) : String := "number";
};
