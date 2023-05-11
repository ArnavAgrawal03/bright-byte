type color =
  | Red
  | Blue
  | Pink
  | Yellow

type t

val init : Board.position -> color -> Command.dir -> t
(** [init p c d] is a ghost with initial position [p], color [c], and pointing
    in direction [d]*)

val pos_of : t -> Board.position
(** [pos_of g] is the current position of ghost [g]*)

val color_of : t -> color
(** [color_of g] is the color of ghost [g]*)

val move : t -> Board.position -> t
(** [move g p] is a ghost [g'] with same attributes as [g] but with current
    position [p]*)
