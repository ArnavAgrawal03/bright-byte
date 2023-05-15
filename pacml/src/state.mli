type t
(** [t] is the type sued to store the current game state information*)

(**The type that represents the game status either playing, won, or lost*)
type game_over =
  | Playing
  | Won
  | Lost

(**The type that represents the difficulty level of the game*)
type difficulty =
  | Easy
  | Normal
  | Hard

val meal_value : int
(**The value representing the cost of a pac dot *)

val big_meal_value : int
(**The value representing the cost of a big pacdot *)

val eat_g_value : int
(**The value representing the cost of a ghost *)

val difficulty_of_string : string -> difficulty
(** [difficulty_of_string s] converts string [s] to the corresponding difficulty
    value.*)

val active_fraction : difficulty -> float
(** [active_fraction d] calculates and returns a float based on the difficulty
    [d].*)

val max_scatter : difficulty -> int
(** [max_scatter d] returns the scatter of ghost based on the difficulty [d].*)

val get_board : t -> Board.t
(**[get_board t] gets the board associated with the state [t]*)

val get_logic : t -> Logic.t
(**[get_logic t] gets the pacman associated with the state [t]*)

val get_ghosts : t -> Ghost.t list
(**[get_ghosts t] gets the ghost list associated with the state [t]*)

val get_lives : t -> int
(**[get_lives t] gets the game lives associated with the state [t]*)

val get_score : t -> int
(**[get_score t] gets the game score associated with the state [t]*)

val is_paused : t -> bool
(**[is_paused t] is a boolean representation of if the game is paused or not*)

val should_quit : t -> bool
(**[should_quit t] is a boolean representation of if the game should quit or not
   and if true then quits the game*)

val get_game_state : t -> game_over
(**[get_game_state t] gets the game state associated with the state [t]*)

val get_exits : t -> Board.position list
(**[get_exits t] gets the exits associated with the state [t]*)

val get_max_orbs : t -> int
(**[get_max_orbs t] gets the maximium number of pac-dots associated with the
   state [t]*)

val get_difficulty : t -> difficulty
(**[get_difficulty t] gets the difficulty associated with the state [t]*)

val origin : int * int
(** The origin point used as a reference in coordinate systems.*)

val setup_point :
  string array array ->
  int ->
  int ->
  ((int * int) * Ghost.color) list ref ->
  Logic.t ref ->
  (int * int) list ref ->
  unit
(** Sets up the game state at a specific point on the board.*)

val start_game : Board.t -> string -> t
(**[start_game b d] initializes a new state based on board [b] and difficulty
   [d]. *)

val init_state :
  Board.t ->
  Logic.t ->
  Ghost.t list ->
  int ->
  int ->
  bool ->
  bool ->
  game_over ->
  Board.position list ->
  int ->
  difficulty ->
  t
(** [init_state] describes the initial state of the board in state *)

val update : t -> Command.command -> t
(**[update t command] updates the state [t] based on the command [command] and
   returns the changed state.*)

val print_game : t -> unit
(**[print_game t] prints the state [t] on the screen.*)

val update_ghost_rep : Ghost.t list -> Board.t -> difficulty -> unit
(** [update_ghost_rep gs arr] modifies the game board to allow for scattered
    ghosts*)
