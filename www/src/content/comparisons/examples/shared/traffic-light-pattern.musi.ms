let TrafficLight := data {
  | Red
  | Yellow
  | Green
};

let canWalk (light : TrafficLight) : Bool :=
  match light (
  | .Green => 0 = 0
  | _ => 0 = 1
  );
