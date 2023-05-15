type t
(** [t] is the type sued to store the values associated with the Pac-Man
    Character*)

val pacman : Board.position -> Command.dir -> Board.position -> t
(** [pacman p d o] is a pacman with position [p], direction [d], and orginal
    position [o] *)

val char_rep : string
(** [char_rep] is the representation of pac-man as a character *)

val position : t -> Board.position
(** [position t] is the current position of pacman *)

val dir : t -> Command.dir
(** [dir t] is the direction pac-man is pointing in *)

val og_pos : t -> Board.position
(** [og_pos t] is the original position of the pac-man character *)

val move_pac : t -> Board.t -> Command.dir -> t

(** [move_pac t b dir] moves the pac-man character by one move in direction
    [dir] on the board [b]*)
val make_og_pos : t -> t
(** [make_og_pos t] moves the pac-man character to its orginal position*)
