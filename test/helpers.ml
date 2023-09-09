let declare_test (tests : (string * (unit -> unit)) list ref) (title : string)
    (executor : unit -> unit) =
  tests := !tests @ [ (title, executor) ]

let check_true received = Alcotest.(check bool) "should be true" true received

let check_false received =
  Alcotest.(check bool) "should be false" false received

let check_int expected received =
  Alcotest.(check int)
    (Printf.sprintf "should be %d" expected)
    expected received

let check_match_some expected_l expected_r received =
  let expected_opt = Some (expected_l, expected_r) in
  Alcotest.(check (option (pair (list char) (list char))))
    "should match" expected_opt received

let check_match_none received =
  Alcotest.(check (option (pair (list char) (list char))))
    "should be None" None received
