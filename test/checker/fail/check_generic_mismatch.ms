val Container<T> := record {
  var value: T;
};

val int_container: Container<Int> := Container{ .value := "wrong_type" };
