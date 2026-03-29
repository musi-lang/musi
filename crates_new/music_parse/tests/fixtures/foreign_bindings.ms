foreign "c" let puts(msg : String) : Int;
foreign "c" (
  let @link(name := "puts") puts(msg : String) : Int;
  let strlen(s : String) : Int;
);
