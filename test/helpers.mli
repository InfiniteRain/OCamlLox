open OCamlLox.Util

val declare_test :
  (string * (unit -> unit)) list ref -> string -> (unit -> unit) -> unit

val check_true : bool -> unit
val check_false : bool -> unit
val check_int : int -> int -> unit
val check_match_some : char list -> char list -> char match_option -> unit
val check_match_none : char match_option -> unit
