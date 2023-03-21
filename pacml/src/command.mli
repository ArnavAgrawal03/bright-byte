(** Parsing of player commands. *)

type dir =
  | Up
  | Left
  | Down
  | Right
      (** the type [dir] represents the direction in which pacman will move*)

type command =
  | Move of dir
  | Pause
  | Quit
      (** The type [command] represents a player command that is decomposed into
          a verb and possibly a direction. Invariant: [Move] must have a
          non-empty direction*)

exception Empty
(** raised when an empty command is parsed*)

exception Malformed
(** raised when a malformed command is parsed*)

val parse : string -> command
<<<<<<< HEAD
(** [parse] [c] is the parsed version of the command [c]. We take a single letter as input,
     and based on that infer the command of the user. The command can be [Move] {[Up], [Left], [Down], [Right]} or [Pause] or [Quit]*)
=======
(** [parse] [c] is the parsed version of the command [c]. We take a single
    letter as input, and based on that infer the command of the user. The
    command can be [Move] ([Up], [Left], [Down], [Right]) or [Pause] or [Quit]*)
>>>>>>> 89c72546f75371fc5ff34cecb660546f3aa41c2e
