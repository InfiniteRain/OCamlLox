open Core

let load_tests tests =
  List.map tests ~f:(fun (name, fn) -> Alcotest.test_case name `Quick fn)
;;

Alcotest.run "OCamlLox" [ ("Util", load_tests !UtilTests.t) ]
