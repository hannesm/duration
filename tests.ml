open Duration

let test_f f s factor upper upperv () =
  List.iter (fun v ->
      Alcotest.(check int64 (s ^ " works" ^ string_of_int v)
                  Int64.(mul (of_int v) factor)
                  (f v)))
    [ 0 ; 1 ; 100 ] ;
  Alcotest.(check_raises (s ^ " raises") (Invalid_argument "negative")
              (fun () -> ignore (f (-1)))) ;
  Alcotest.(check int64 ("upper bound " ^ s ^ " good")
              upperv (f upper)) ;
  Alcotest.(check_raises ("upper bound + 1 " ^ s ^ " raises")
              (Invalid_argument "out of range")
              (fun () -> ignore (f (succ upper))))

let test_g g s factor upper upperv () =
  List.iter (fun (e, v) ->
      Alcotest.(check int (s ^ " works " ^ Int64.to_string v)
                  e (g v)))
    [ 0, 0L ; 1, factor ] ;
  Alcotest.(check int ("upper bound " ^ s ^ " good")
              upper (g upperv))

let test_inv f g s upper () =
  Alcotest.(check int ("inverse 0 " ^ s) 0 (g (f 0))) ;
  Alcotest.(check int ("inverse 1 " ^ s) 1 (g (f 1))) ;
  Alcotest.(check int ("inverse 10 " ^ s) 10 (g (f 10))) ;
  Alcotest.(check int "inverse upper" upper (g (f upper)))

let test_one f g s fa m mv = [
  "of" ^ s ^ " is good", `Quick, test_f f ("of" ^ s) fa m mv ;
  "to" ^ s ^ " is good", `Quick, test_g g ("to" ^ s) fa m mv ;
  "inverse of/to" ^ s, `Quick, test_inv f g s m
]

let test_f_64 f s factor upper upperv () =
  List.iter (fun v ->
      Alcotest.(check int64 (s ^ " works" ^ Int64.to_string v)
                  Int64.(mul v factor)
                  (f v)))
    [ 0L ; 1L ; 100L ] ;
  Alcotest.(check_raises (s ^ " raises") (Invalid_argument "negative")
              (fun () -> ignore (f (-1L)))) ;
  Alcotest.(check int64 ("upper bound " ^ s ^ " good")
              upperv (f upper)) ;
  Alcotest.(check_raises ("upper bound + 1 " ^ s ^ " raises")
              (Invalid_argument "out of range")
              (fun () -> ignore (f (Int64.succ upper))))

let test_g_64 g s factor upper upperv () =
  List.iter (fun (e, v) ->
      Alcotest.(check int64 (s ^ " works " ^ Int64.to_string v)
                  e (g v)))
    [ 0L, 0L ; 1L, factor ] ;
  Alcotest.(check int64 ("upper bound " ^ s ^ " good")
              upper (g upperv))

let test_inv_64 f g s upper () =
  Alcotest.(check int64 ("inverse 0 " ^ s) 0L (g (f 0L))) ;
  Alcotest.(check int64 ("inverse 1 " ^ s) 1L (g (f 1L))) ;
  Alcotest.(check int64 ("inverse 10 " ^ s) 10L (g (f 10L))) ;
  Alcotest.(check int64 "inverse upper" upper (g (f upper)))

let test_one_64 f g s fa m mv = [
  "of" ^ s ^ " is good", `Quick, test_f_64 f ("of" ^ s) fa m mv ;
  "to" ^ s ^ " is good", `Quick, test_g_64 g ("to" ^ s) fa m mv ;
  "inverse of/to" ^ s, `Quick, test_inv_64 f g s m
]

let test_of_us =
  let a, b = 18446744073709549L, 0xFFFFFFFFFFFFF5C8L in
  test_one_64 of_us_64 to_us_64 "_us" 1_000L a b @
  if Sys.word_size = 64 then
    test_one of_us to_us "_us" 1_000L (Int64.to_int a) b
  else
    []

let test_of_ms =
  let a, b = 18446744073709L, 0xFFFFFFFFFFF79540L in
  test_one_64 of_ms_64 to_ms_64 "_ms" 1_000_000L a b @
  if Sys.word_size = 64 then
    test_one of_ms to_ms "_ms" 1_000_000L (Int64.to_int a) b
  else
    []

let test_of_sec =
  let a, b = 18446744073L, 0xFFFFFFFFD5B51A00L in
  test_one_64 of_sec_64 to_sec_64 "_sec" 1_000_000_000L a b @
  if Sys.word_size = 64 then
    test_one of_sec to_sec "_sec" 1_000_000_000L (Int64.to_int a) b
  else
    []

let test_of_min =
  test_one of_min to_min "_min" 60_000_000_000L 307445734 0xFFFFFFF826C11000L

let test_of_hour =
  test_one of_hour to_hour "_hour" 3600_000_000_000L 5124095 0xFFFFFE1D2D476000L

let test_of_day =
  let d_in_ns = Int64.mul 24L 3600_000_000_000L in
  test_one of_day to_day "_day" d_in_ns 213503 0xFFFFB2CECCB10000L

let test_of_year =
  let y_in_ns = Int64.mul 8766L 3600_000_000_000L in
  test_one of_year to_year "_year" y_in_ns 584 0xFFC33A7AFAE60000L


let test_of_f () =
  List.iter (fun v ->
      Alcotest.(check int64 ("of_f works " ^ string_of_float v)
                  (Int64.of_float (v *. 1_000_000_000.))
                  (of_f v)))
    [ 0. ; 1. ; 2. ; 0.000000001 ] ;
  Alcotest.(check_raises ("of_f raises") (Invalid_argument "negative")
              (fun () -> ignore (of_f (-1.)))) ;
  Alcotest.(check int64 ("upper bound of_f good")
              0xFFFFFFFFFFFFF596L (of_f 18446744073.709549)) ;
  Alcotest.(check_raises ("upper bound + 1 of_f raises")
              (Invalid_argument "out of range")
              (fun () -> ignore (of_f 18446744073.709551)))

let test_to_f () =
  List.iter (fun (e, v) ->
      Alcotest.(check (float 0.) ("to_f works " ^ Int64.to_string v)
                  e (to_f v)))
    [ 0., 0L ; 1., 1000000000L ; 2., 2000000000L ; 0.000000001, 1L ] ;
  Alcotest.(check (float 0.) ("upper bound to_f good")
              18446744073.709549 (to_f 0xFFFFFFFFFFFFF596L))

let test_inv_f () =
  Alcotest.(check (float 0.) "inverse 0 to/of_f" 0. (to_f (of_f 0.))) ;
  Alcotest.(check (float 0.) "inverse 1 to/of_f" 1. (to_f (of_f 1.))) ;
  Alcotest.(check (float 0.) "inverse 10 to/of_f" 10. (to_f (of_f 10.))) ;
  Alcotest.(check (float 0.) "inverse 3.5 to/of_f" 3.5 (to_f (of_f 3.5))) ;
  Alcotest.(check (float 0.) "inverse upper" 18446744073.709549 (to_f (of_f 18446744073.709549)))

let test_float = [
  "of_f is good", `Quick, test_of_f ;
  "to_f is good", `Quick, test_to_f ;
  "inverse of/to_f", `Quick, test_inv_f
]

let test_of_string =
  Alcotest.test_case "of_string_exn" `Quick @@ fun () ->
  Alcotest.(check int64) "1s" (Duration.of_string_exn "1s") (Duration.of_sec 1);
  Alcotest.(check int64) "1m" (Duration.of_string_exn "1m") (Duration.of_min 1);
  Alcotest.(check int64) "1h" (Duration.of_string_exn "1h") (Duration.of_hour 1);
  Alcotest.(check int64) "42d" (Duration.of_string_exn "42d") (Duration.of_day 42);
  Alcotest.(check int64) "1y" (Duration.of_string_exn "1y") (Duration.of_year 1);
  Alcotest.(check int64) "1ms" (Duration.of_string_exn "1ms") (Duration.of_ms 1);
  Alcotest.(check int64) "1ns" (Duration.of_string_exn "1ns") 1L;
  Alcotest.(check int64) "0s" (Duration.of_string_exn "0s") 0L;
  Alcotest.(check int64) "0ns" (Duration.of_string_exn "0ns") 0L;
  Alcotest.(check int64) "100ms" (Duration.of_string_exn "100ms") (Duration.of_ms 100);
  Alcotest.(check int64) "500ns" (Duration.of_string_exn "500ns") 500L;
  Alcotest.(check int64) "10h" (Duration.of_string_exn "10h") (Duration.of_hour 10);
  Alcotest.(check int64) "42a" (Duration.of_string_exn "42a") (Duration.of_year 42);
  Alcotest.(check int64) "11us" (Duration.of_string_exn "11us") (Duration.of_us 11);
  Alcotest.(check int64) "12µs (micro sign)" (Duration.of_string_exn "12µs") (Duration.of_us 12);
  Alcotest.(check int64) "13μs (greek small letter mu)" (Duration.of_string_exn "13μs") (Duration.of_us 13)

let test_of_string_composite =
  let ( + ) = Int64.add in
  Alcotest.test_case "of_string_exn: composite" `Quick @@ fun () ->
  Alcotest.(check int64) "1h30m15s"
    (Duration.of_string_exn "1h30m15s")
    (Duration.of_hour 1 + Duration.of_min 30 + Duration.of_sec 15);
  Alcotest.(check int64) "100ms500ns"
    (Duration.of_string_exn "100ms500ns")
    (Duration.of_ms 100 + 500L);
  Alcotest.(check int64) "1m1s = 1s1m (order independence)"
    (Duration.of_string_exn "1m1s")
    (Duration.of_string_exn "1s1m");
  Alcotest.(check int64) "all metrics: 1y1d1h1m1s1ms1ns"
    (Duration.of_string_exn "1y1d1h1m1s1ms1ns")
    (Duration.of_year 1 + Duration.of_day 1 + Duration.of_hour 1 +
     Duration.of_min 1 + Duration.of_sec 1 + Duration.of_ms 1 + 1L);
  Alcotest.(check int64) "1m30s"
    (Duration.of_string_exn "1m30s")
    (Duration.of_min 1 + Duration.of_sec 30);
  Alcotest.(check int64) "1d12h"
    (Duration.of_string_exn "1d12h")
    (Duration.of_day 1 + Duration.of_hour 12);
  Alcotest.(check int64) "1m1ms"
    (Duration.of_string_exn "1m1ms")
    (Duration.of_min 1 + Duration.of_ms 1);
  Alcotest.(check int64) "1s1ns"
    (Duration.of_string_exn "1s1ns")
    (Duration.of_sec 1 + 1L);
  Alcotest.(check int64) "1ms1s (ms before s)"
    (Duration.of_string_exn "1ms1s")
    (Duration.of_ms 1 + Duration.of_sec 1);
  Alcotest.(check int64) "1ns1ms1s (ns, ms, s)"
    (Duration.of_string_exn "1ns1ms1s")
    (1L + Duration.of_ms 1 + Duration.of_sec 1)

let check_raises_exn msg f =
  let raised = try f (); false with _ -> true in
  Alcotest.(check bool) msg true raised

let test_of_string_duplicate_errors =
  Alcotest.test_case "of_string_exn: duplicate metrics" `Quick @@ fun () ->
  let invalid_argf fmt = Fmt.kstr (fun msg -> Invalid_argument msg) fmt in
  Alcotest.check_raises "duplicate d" (invalid_argf "Multiple use of the metric 'd'")
    (fun () -> ignore (Duration.of_string_exn "1d1d"));
  Alcotest.check_raises "duplicate s" (invalid_argf "Multiple use of the metric 's'")
    (fun () -> ignore (Duration.of_string_exn "1s1s"));
  Alcotest.check_raises "duplicate ms" (invalid_argf "Multiple use of the metric 'ms'")
    (fun () -> ignore (Duration.of_string_exn "1ms1ms"));
  Alcotest.check_raises "duplicate ns" (invalid_argf "Multiple use of the metric 'ns'")
    (fun () -> ignore (Duration.of_string_exn "1ns1ns"))

let test_of_string_invalid_metric =
  Alcotest.test_case "of_string_exn: invalid metric" `Quick @@ fun () ->
  let invalid_argf fmt = Fmt.kstr (fun msg -> Invalid_argument msg) fmt in
  Alcotest.check_raises "metric x" (invalid_argf "Invalid metric: \"1x\"")
    (fun () -> ignore (Duration.of_string_exn "1x"));
  Alcotest.check_raises "metric w" (invalid_argf "Invalid metric: \"1w\"")
    (fun () -> ignore (Duration.of_string_exn "1w"));
  Alcotest.check_raises "metric mn" (invalid_argf "Invalid metric: \"1mn\"")
    (fun () -> ignore (Duration.of_string_exn "1mn"));
  Alcotest.check_raises "metric D (uppercase)" (invalid_argf "Invalid metric: \"1D\"")
    (fun () -> ignore (Duration.of_string_exn "1D"))

let test_of_string_malformed =
  Alcotest.test_case "of_string_exn: malformed input" `Quick @@ fun () ->
  let invalid_argf fmt = Fmt.kstr (fun msg -> Invalid_argument msg) fmt in
  Alcotest.check_raises "only metric d" (invalid_argf "Invalid metric: \"d\"")
    (fun () -> ignore (Duration.of_string_exn "d"));
  Alcotest.check_raises "only metric s" (invalid_argf "Invalid metric: \"s\"")
    (fun () -> ignore (Duration.of_string_exn "s"));
  Alcotest.check_raises "only number" (invalid_argf "Invalid metric: \"123\"")
    (fun () -> ignore (Duration.of_string_exn "123"));
  Alcotest.check_raises "trailing number" (invalid_argf "Invalid metric: \"1s2\"")
    (fun () -> ignore (Duration.of_string_exn "1s2"));
  Alcotest.check_raises "negative" (invalid_argf "Invalid metric: \"-1d\"")
    (fun () -> ignore (Duration.of_string_exn "-1d"));
  Alcotest.check_raises "decimal" (invalid_argf "Invalid metric: \"1.5s\"")
    (fun () -> ignore (Duration.of_string_exn "1.5s"));
  Alcotest.check_raises "spaces" (invalid_argf "Invalid metric: \"1 d\"")
    (fun () -> ignore (Duration.of_string_exn "1 d"));
  Alcotest.check_raises "metric before number" (invalid_argf "Invalid metric: \"d1\"")
    (fun () -> ignore (Duration.of_string_exn "d1"));
  Alcotest.check_raises "letters only" (invalid_argf "Invalid metric: \"abc\"")
    (fun () -> ignore (Duration.of_string_exn "abc"))

let test_of_string_overflow =
  Alcotest.test_case "of_string_exn: out of range" `Quick @@ fun () ->
  let invalid_argf fmt = Fmt.kstr (fun msg -> Invalid_argument msg) fmt in
  Alcotest.check_raises "days out of range" (invalid_argf "out of range")
    (fun () -> ignore (Duration.of_string_exn "999999d"));
  Alcotest.check_raises "years out of range" (invalid_argf "out of range")
    (fun () -> ignore (Duration.of_string_exn "1000y"))

let test_of_string_empty =
  Alcotest.test_case "of_string_exn: empty string" `Quick @@ fun () ->
  let invalid_argf fmt = Fmt.kstr (fun msg -> Invalid_argument msg) fmt in
  Alcotest.check_raises "empty string should raise" (invalid_argf "Invalid metric: \"\"")
    (fun () -> ignore (Duration.of_string_exn ""))

let test_string =
  [ test_of_string; test_of_string_composite; test_of_string_duplicate_errors
  ; test_of_string_invalid_metric; test_of_string_malformed
  ; test_of_string_overflow; test_of_string_empty ]

let dur_tests =
  List.flatten [
    test_of_us ;
    test_of_ms ;
    test_of_sec ;
    test_of_min ;
    test_of_hour ;
    test_of_day ;
    test_of_year ;
    test_float ;
    test_string ;
  ]


let tests = [
  "Duration", dur_tests
]

let () = Alcotest.run "Duration tests" tests
