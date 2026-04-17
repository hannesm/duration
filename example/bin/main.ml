(* Duration provides a way of storing durations from one nanosecond to 584
   years in a 64 bit integer. Note that it does not know the time, or lengths
   of months, or clock changes, or about leap years, and so on. It believes
   that there are 8766 hours in a year. *)

let () =
  Printf.printf "112 days is %Li\n"
    (Duration.of_day 112);
  Printf.printf "21 hours is %Li\n"
    (Duration.of_hour 21);
  Printf.printf "112 days and 21 hours is %Li\n"
    (Int64.add (Duration.of_day 112) (Duration.of_hour 21));
  Printf.printf "Which is %Li milliseconds\n"
    (Duration.to_ms_64 (Int64.add (Duration.of_day 112) (Duration.of_hour 21)));
  (* Use the formatter to print to stdout *)
  Duration.pp
    Format.std_formatter
    (Int64.add (Duration.of_day 112) (Duration.of_hour 21));
  Format.pp_print_newline Format.std_formatter ();
  (* Represented as a float *)
  Printf.printf "As a float: %f\n" (Duration.to_f (Duration.of_hour 20))
