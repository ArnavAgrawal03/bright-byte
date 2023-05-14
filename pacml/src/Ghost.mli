type color =
  | Red
  | Blue
  | Pink
  | Yellow

type t

(*-------- Constructors --------*)
val init : Board.position -> color -> Command.dir -> bool -> t
(** [init p c d a] is a ghost with position [p], color [c], and direction [d]
    and is active if [a] is true. Scatter is false by default, scatter frames is
    0, starting position and current position are the same*)

val inits : (Board.position * color) list -> t list
(** [inits l] is a list of ghosts with positions and colors as specified in [l].
    Default direction and active status is [Up] and [false] respectively. Other
    defaults as in [init]*)

(*-------- Querying functions --------*)
val pos : t -> Board.position
(** [pos g] is the current position of ghost [g]*)

val color : t -> color
(** [color g] is the color of ghost [g]*)

val dir : t -> Command.dir
(** [dir g] is the direction ghost [g] is pointing in*)

val is_active : t -> bool
(** [is_active g] is whether ghost [g] is active*)

val is_scatter : t -> bool
(** [scatter g] is whether ghost [g] is in scatter mode*)

val scatter_frames : t -> int
(** [scatter_frames g] is the number of frames that [g] has been in scatter mode*)

val target : t -> Logic.t -> t list -> Board.t -> Board.position
(** [target g p gs b] is the target position of ghost [g] given states of pacman
    [p] ghosts [gs] and board [b]*)

(*-------- Tranforming functions --------*)
val activate : t -> t
(** [activate g] is a ghost [g'] with same attributes as [g] but active*)

val deactivate : t -> t
(** [deactivate g] is a ghost [g'] with same attributes as [g] but inactive*)

val scatter : t -> t
(** [scatter g] is a ghost [g'] with same attributes as [g] but in scatter mode
    Requires: [is_scatter g] is [false]*)

val unscatter : t -> t
(** [unscatter g] is a ghost [g'] with same attributes as [g] but not in scatter
    mode. Requires: [is_scatter g] is [true]*)

val move_target : Logic.t -> Board.t -> t list -> Board.position list -> t -> t
(** [move_target s b gs ps g] is a ghost [g'] with same attributes as [g] but
    moved to the target position - taking into account the positions of other
    movers such as the pacman and other ghosts*)

val turn : t -> Command.dir -> t
(** [turn g d] is a ghost [g'] with same attributes as [g] but pointing in
    direction [d]*)

val rev : t -> t
(** [rev g] is a ghost [g'] with same attributes as [g] but pointing in the
    reverse direction*)

val move : t -> int -> Command.dir -> t
(** [move g s d] is a ghost [g'] with same attributes as [g] but moved [s]
    spaces in direction [d]*)
