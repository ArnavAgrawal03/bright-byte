type t
(** [t] is the type used to store the current game state information*)

type current_game =
  | Won
  | Lost
  | Play

type difficulty =
  | Easy
  | Medium
  | Hard

val init_state : Csv.t -> t
(***)

val current : t -> int * int
(***)

val lives : t -> int
(***)

val cherries : t -> (int * int) list
(***)

val ghosts : t -> (int * int) list
(***)

val board : t -> string array array
(***)
