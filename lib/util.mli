type 'a match_option = ('a list * 'a list) option

val is_some_value : 'a -> 'a option -> bool
val list_count_value : 'a -> 'a list -> int
val list_match_until : 'a list -> 'a list -> 'a match_option

val list_match_pred_until :
  ('a option -> bool) list -> 'a list -> 'a match_option
