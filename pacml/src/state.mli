type t
val init_state : Csv.t -> t
val current : t -> int*int
val lives: t -> int
val cherries: t -> (int*int) list
val ghosts: t -> (int*int) list
val board: t -> string list list
