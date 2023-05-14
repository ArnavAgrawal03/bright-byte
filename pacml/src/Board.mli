(** Representation of the board*)

(*The abstract type of the values in the board.*)
type t = string array array

(**)
type position = int * int

val border : string
val pac_dots : string
val empty : string
val is_border : t -> position -> bool
val board_array : t -> string array array
val num_dots_left : string array array -> int
val update_empty_dot : position -> t -> t
val won : t -> bool
val got_dot : position -> t -> bool
val move_pos : position -> Command.dir -> position
val board_display : t -> t
val csv_array : Csv.t -> t
