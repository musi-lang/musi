let status_prefix status =
  Fmt.str "%a" Fmt.(styled `Bold string) (Printf.sprintf "%12s" status)

let compiling input output =
  Printf.printf
    "%s %s -> %s\n"
    (status_prefix
       (Fmt.str
          "%a"
          Fmt.(styled `Bold (styled (`Fg `Green) string))
          "Compiling"))
    input
    output;
  flush stdout

let finished profile count time =
  Printf.printf
    "%s %s target%s in %s\n"
    (status_prefix
       (Fmt.str "%a" Fmt.(styled `Bold (styled (`Fg `Green) string)) "Finished"))
    profile
    (if count = 1 then "" else "s")
    time;
  flush stdout

let checking file =
  Printf.printf
    "%s %s\n"
    (status_prefix
       (Fmt.str "%a" Fmt.(styled `Bold (styled (`Fg `Green) string)) "Checking"))
    file;
  flush stdout

let error msg =
  Printf.eprintf
    "%s %s\n"
    (status_prefix
       (Fmt.str "%a" Fmt.(styled `Bold (styled (`Fg `Red) string)) "Error"))
    msg;
  flush stderr

let warning msg =
  Printf.eprintf
    "%s %s\n"
    (status_prefix
       (Fmt.str "%a" Fmt.(styled `Bold (styled (`Fg `Yellow) string)) "Warning"))
    msg;
  flush stderr

let running file =
  Printf.printf
    "%s %s\n"
    (status_prefix
       (Fmt.str "%a" Fmt.(styled `Bold (styled (`Fg `Green) string)) "Running"))
    file;
  flush stdout
