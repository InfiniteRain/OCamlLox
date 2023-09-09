open Core
open Helpers
open OCamlLox.Util
open Alcotest

let t = ref [];;

declare_test t "is_some_value works as expected" (fun () ->
    check_true (is_some_value "val" (Some "val")));

declare_test t "list_count_value works on empty lists" (fun () ->
    check_int 0 (list_count_value "val" []));

declare_test t "list_count_value works when no values matched" (fun () ->
    check_int 0 (list_count_value "val" [ "wrong"; "wrong"; "wrong"; "wrong" ]));

declare_test t "list_count_value works when some value matched" (fun () ->
    check_int 2
      (list_count_value "val" [ "wrong"; "wrong"; "val"; "wrong"; "val" ]));

declare_test t "list_match_until works with an empty pattern and an empty list"
  (fun () ->
    let opt = list_match_until [] [] in
    check_match_some [] [] opt);

declare_test t
  "list_match_until works with an empty pattern and a non-empty list" (fun () ->
    let lst = String.to_list "Hello" in
    let opt = list_match_until [] lst in
    check_match_some [] lst opt);

declare_test t
  "list_match_until works with a non-empty pattern and an empty list" (fun () ->
    let pat = [ 'h' ] in
    let opt = list_match_until pat [] in
    check_match_none opt);

declare_test t "list_match_until works when match isn't found" (fun () ->
    let lst = String.to_list "Hello" in
    let opt = list_match_until [ 'd' ] lst in
    check_match_none opt);

declare_test t "list_match_until works when pattern larger than list" (fun () ->
    let lst = String.to_list "Hello" in
    let pat = String.to_list "Much larger" in
    let opt = list_match_until pat lst in
    check_match_none opt);

declare_test t "list_match_until works when match is found" (fun () ->
    let lst = String.to_list "end of a string\"; something else" in
    let opt = list_match_until [ '"' ] lst in
    let expected_l = String.to_list "end of a string\"" in
    let expected_r = String.to_list "; something else" in
    check_match_some expected_l expected_r opt);

declare_test t "list_match_pred_until works with unescaped pattern" (fun () ->
    let lst = String.to_list "string \"; something else" in
    let pat = [ Fn.non (is_some_value '\\'); is_some_value '"' ] in
    let opt = list_match_pred_until pat lst in
    let expected_l = String.to_list "string \"" in
    let expected_r = String.to_list "; something else" in
    check_match_some expected_l expected_r opt);

declare_test t "list_match_pred_until works with escaped pattern" (fun () ->
    let lst = String.to_list "string \\\" ends here\"; something else" in
    let pat = [ Fn.non (is_some_value '\\'); is_some_value '"' ] in
    let opt = list_match_pred_until pat lst in
    let expected_l = String.to_list "string \\\" ends here\"" in
    let expected_r = String.to_list "; something else" in
    check_match_some expected_l expected_r opt);

declare_test t "list_match_pred_until works with instantly unescaped pattern"
  (fun () ->
    let lst = String.to_list "\"; something else" in
    let pat = [ Fn.non (is_some_value '\\'); is_some_value '"' ] in
    let opt = list_match_pred_until pat lst in
    let expected_l = [ '"' ] in
    let expected_r = String.to_list "; something else" in
    check_match_some expected_l expected_r opt);

declare_test t "list_match_pred_until works with instantly escaped pattern"
  (fun () ->
    let lst = String.to_list "\\\" ends here\"; something else" in
    let pat = [ Fn.non (is_some_value '\\'); is_some_value '"' ] in
    let opt = list_match_pred_until pat lst in
    let expected_l = String.to_list "\\\" ends here\"" in
    let expected_r = String.to_list "; something else" in
    check_match_some expected_l expected_r opt);

declare_test t "list_match_pred_until works with suffix pattern" (fun () ->
    let lst = String.to_list "ends with # here" in
    let pat = [ is_some_value '#'; Fn.non (is_some_value '*') ] in
    let opt = list_match_pred_until pat lst in
    let expected_l = String.to_list "ends with # " in
    let expected_r = String.to_list "here" in
    check_match_some expected_l expected_r opt);

declare_test t "list_match_pred_until works with suffix pattern at the end"
  (fun () ->
    let lst = String.to_list "ends with #" in
    let pat = [ is_some_value '#'; Fn.non (is_some_value '*') ] in
    let opt = list_match_pred_until pat lst in
    let expected_l = String.to_list "ends with #" in
    let expected_r = [] in
    check_match_some expected_l expected_r opt);

declare_test t
  "list_match_pred_until works with escaped suffix pattern at the end"
  (fun () ->
    let lst = String.to_list "ends with #*" in
    let pat = [ is_some_value '#'; Fn.non (is_some_value '*') ] in
    let opt = list_match_pred_until pat lst in
    check_match_none opt)
