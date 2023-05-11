(** Representation of the board*)

(*The abstract type of the values in the board.*)
type t

(**)
type position = int * int

val boarder : char
val pac_dots : char
val empty : char
val is_border : t -> position -> bool
val board_array : t -> string array array
val num_dots_left : t -> int
val update_empty_dot : position -> t -> t
val won : t -> bool
val got_dot : position -> t -> bool
val move_pos : position -> Command.dir -> t
val board_display : t -> t
