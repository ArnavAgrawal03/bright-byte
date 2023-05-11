type t

val pacman : Board.position -> Command.dir -> Board.position -> t
val char_rep : string
val position : t -> Board.position
val dir : t -> Command.dir
val og_pos : t -> Board.position
val move_pac : t -> Board.t -> Command.dir -> t
val make_og_pos : t -> t
