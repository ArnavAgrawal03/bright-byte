type t = string array array
(**The abstract type of the values in the board.*)

type position = int * int
(**The represenation of a 2D position with x and y coordinate. *)

val border : string
(** [border] is a string representing the border of the games. *)

val pac_dots : string
(** [pac_dots] is a string representing the pac-dots on the board. *)

val big_pac_dots : string
(** [big_pac_dots] is a string representing the bigger pac-dots on the board
    that allow for scatter state. *)

val empty : string
(** [empty] is a string representing the empty spaces on the board. *)

val container_col : string
(** [container_col] is a string representing the right and left walls of the
    container holding the ghosts. *)

val container_row : string
(** [container_row] is a string representing the top and bottom walls of the
    container holding the ghosts. *)

val exit_row : string
(** [exit_row] is a string representing the exit of the container holding the
    ghosts. *)

val is_border : t -> position -> bool
(**[is_border t p] checks is position [p] is a border on the board [t]*)

val is_container : t -> position -> bool
(**[is_container t p] checks is position [p] is a container of ghosts on the
   board [t]*)

val is_container_exit : t -> position -> bool
(**[is_container_exits t p] checks is position [p] is an exit of container of
   ghosts on the board [t]*)

val board_array : t -> string array array
(**[board_array t] represents board [t] as an array of array of strings*)

val num_dots_left : string array array -> int
(**[num_dots_left t] gives how many pac-dots are still on board [t]*)

val update_empty_dot : position -> t -> t
(**[update_empty_dot p t] remove the pac-dot at position [p] on board [t]*)

val won : t -> bool
(**[won t] checks if there are pac-dots remaining and returns true if there are
   none else false*)

val got_dot : position -> t -> bool
(**[got_dot p t] returns true if there is a pac-dot at position [p] on board [t]
   else false*)

val got_big_dot : position -> t -> bool
(**[got_big_dot p t] returns true if there is a big pac-dot at position [p] on
   board [t] else false*)

val move_pos : position -> Command.dir -> position
(**[move_pos p dir] changes the current position [p] by 1 step in the dirrection
   [dir]*)

val board_display : t -> t
(**[board_display t] represents the board as an array which only contains the
   ghosts and walls*)

val csv_array : Csv.t -> t
(**[csv_array t] gives the array of the csv file [t]*)
