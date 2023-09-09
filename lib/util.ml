open Core

type 'a match_option = ('a list * 'a list) option

let is_some_value value opt = Poly.( = ) opt (Some value)
let list_count_value value lst = List.count lst ~f:(Poly.( = ) value)

let list_unwrap_options lst =
  let rec aux lst acc =
    match lst with
    | h :: t -> ( match h with Some a -> aux t (a :: acc) | None -> aux t acc)
    | [] -> List.rev acc
  in
  aux lst []

let rec list_match_prefix prefix lst =
  match (prefix, lst) with
  | [], _ -> true
  | ph :: pt, lh :: lt when ph lh -> list_match_prefix pt lt
  | _ -> false

let rec list_match_pred_until_aux left ind pat lst =
  match lst with
  | [] when List.is_empty pat -> Some ([], [])
  | [] -> None
  | _ :: _ as lst when list_match_prefix pat lst ->
      let length = List.length pat in
      let prefix = List.sub lst ~pos:0 ~len:length in
      Some (List.rev_append left prefix, List.drop lst length)
  | lh :: lt -> list_match_pred_until_aux (lh :: left) (ind + 1) pat lt

let list_match_until pat lst =
  list_match_pred_until_aux [] 0
    (List.map pat ~f:is_some_value)
    (List.map lst ~f:Option.some)
  |> Option.map ~f:(fun (a, b) ->
         (list_unwrap_options a, list_unwrap_options b))

let list_match_pred_until pat lst =
  let pad =
    match pat with
    | [] -> []
    | _ :: _ -> List.init (List.length pat - 1) ~f:(fun _ -> None)
  in
  let padded_lst = pad @ List.map lst ~f:Option.some @ pad in
  list_match_pred_until_aux [] 0 pat padded_lst
  |> Option.map ~f:(fun (a, b) ->
         (list_unwrap_options a, list_unwrap_options b))
