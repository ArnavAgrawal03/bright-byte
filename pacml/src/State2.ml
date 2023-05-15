open ANSITerminal

type game_over =
  | Playing
  | Won
  | Lost

type difficulty =
  | Easy
  | Normal
  | Hard

let orb_value : int = 10
let big_orb_value : int = 50
let ghost_value : int = 200

let difficulty_of_string (str : string) : difficulty =
  match String.lowercase_ascii (String.trim str) with
  | "e" | "easy" | "1" -> Easy
  | "n" | "normal" | "2" -> Normal
  | "h" | "hard" | "3" -> Hard
  | _ -> failwith "Invalid difficulty"

let active_fraction (difficulty : difficulty) : float =
  match difficulty with
  | Easy -> 1. /. 4.
  | Normal -> 1. /. 6.
  | Hard -> 1. /. 8.

let scatter_frames (difficulty : difficulty) : int =
  match difficulty with
  | Easy -> 20
  | Normal -> 10
  | Hard -> 8

type t = {
  board : Board.t;
  pacman : Logic.t;
  ghosts : Ghost.t list;
  lives : int;
  score : int;
  paused : bool;
  quit_game : bool;
  game_state : game_over;
  exit_tiles : Board.position list;
  max_orbs : int;
  difficulty : difficulty;
}

let get_board (game : t) : Board.t = game.board
let get_logic (game : t) : Logic.t = game.pacman
let get_ghosts (game : t) : Ghost.t list = game.ghosts
let get_lives (game : t) : int = game.lives
let get_score (game : t) : int = game.score
let is_paused (game : t) : bool = game.paused
let should_quit (game : t) : bool = game.quit_game
let get_game_state (game : t) : game_over = game.game_state
let get_exit_tiles (game : t) : Board.position list = game.exit_tiles
let get_max_orbs (game : t) : int = game.max_orbs
let get_difficulty (game : t) : difficulty = game.difficulty
let origin = (0, 0)

let setup_point board height width ghost_data pac exits =
  let coordinates = (height, width) in
  match board.(height).(width) with
  | "R" -> ghost_data := (coordinates, Ghost.Red) :: !ghost_data
  | "B" -> ghost_data := (coordinates, Blue) :: !ghost_data
  | "P" -> ghost_data := (coordinates, Pink) :: !ghost_data
  | "Y" -> ghost_data := (coordinates, Yellow) :: !ghost_data
  | "C" -> pac := Logic.pacman (height, width) Right (height, width)
  | "_" -> exits := coordinates :: !exits
  | _ -> failwith "ABC"

let start_game board level =
  let ghost_data = ref [] in
  let pac = ref (Logic.pacman origin Right origin) in
  let exits = ref [] in
  for height = 0 to Array.length board - 1 do
    for width = 0 to Array.length board.(height) - 1 do
      setup_point board height width ghost_data pac exits
    done
  done
  |> ignore;
  {
    board;
    pacman = !pac;
    ghosts = board |> Ghost.init_random !ghost_data;
    lives = 3;
    score = 0;
    paused = false;
    quit_game = false;
    game_state = Playing;
    exit_tiles = !exits;
    max_orbs = !ghost_data |> List.length;
    difficulty = level |> difficulty_of_string;
  }
