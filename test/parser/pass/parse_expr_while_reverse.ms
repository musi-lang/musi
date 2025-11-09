const count := proc (limit: Int) {
  var i := 0;
  do {
    i <- i + 1;
  } while i < limit;
};

var x := 0;
do {
  x <- x + 1;
} while x < 5;
