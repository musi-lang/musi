let Box [T] := data {
  value : T;
};

let boxedName : Box[String] := {
  value := "Nora"
};
boxedName.value;
