type game_over =
  | Playing
  | Won
  | Lost

type difficulty =
  | Easy
  | Normal
  | Hard

let meal_value : int = 10
let big_meal_value : int = 50
let eat_g_value : int = 200

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

let max_scatter (difficulty : difficulty) : int =
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
  exits : Board.position list;
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
let get_exits (game : t) : Board.position list = game.exits
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
  | _ -> ()

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
    exits = !exits;
    max_orbs = !ghost_data |> List.length;
    difficulty = level |> difficulty_of_string;
  }

let move_pac dir game =
  { game with pacman = Logic.move_pac game.pacman game.board dir }

let moved_ghosts game ghosts =
  ghosts |> List.map (Ghost.move game.pacman game.board game.ghosts game.exits)

let move_ghosts game = { game with ghosts = game.ghosts |> moved_ghosts game }

let scatter_incr_single g =
  let open Ghost in
  if is_scatter g then incr_scatter_frames g else g

let update_scatter_frames game =
  { game with ghosts = game.ghosts |> List.map scatter_incr_single }

let eaten game =
  {
    game with
    lives = game.lives - 1;
    ghosts = List.map Ghost.unscatter (List.map Ghost.reset game.ghosts);
    pacman = Logic.make_og_pos game.pacman;
  }

let reverse = function
  | Command.Up -> Command.Down
  | Down -> Up
  | Left -> Right
  | Right -> Left

let through pac g =
  let opposed = Ghost.dir g = reverse (Logic.dir pac) in
  let passed =
    Logic.position pac = Board.move_pos (Ghost.pos g) (Logic.dir pac)
  in
  opposed && passed

let collide_single pac g =
  ((not (Ghost.is_scatter g)) && Ghost.pos g = Logic.position pac)
  || through pac g

let collide_with_any pac ghosts =
  ghosts |> List.map (collide_single pac) |> List.fold_left ( || ) false

let reset_single_g pac g =
  let col_a = Ghost.is_scatter g && Ghost.pos g = Logic.position pac in
  let col_b = through pac g in
  if col_a || col_b then g |> Ghost.reset |> Ghost.unscatter else g

let reset_ghosts pac ghosts = ghosts |> List.map (reset_single_g pac)

let eat_g_score pac g =
  if (Ghost.is_scatter g && Ghost.pos g = Logic.position pac) || through pac g
  then eat_g_value
  else 0

let eat_gs_score pac gs =
  gs |> List.map (eat_g_score pac) |> List.fold_left ( + ) 0

let process_cols game =
  if collide_with_any game.pacman game.ghosts then eaten game
  else
    let gs' = reset_ghosts game.pacman game.ghosts in
    let score' = game.score + eat_gs_score game.pacman game.ghosts in
    { game with ghosts = gs'; score = score' }

let eat_big_meal game =
  {
    game with
    ghosts = List.map Ghost.scatter (List.map Ghost.rev game.ghosts);
    score = big_meal_value + game.score;
    board = Board.update_empty_dot (Logic.position game.pacman) game.board;
  }

let eat_meal game =
  {
    game with
    score = meal_value + game.score;
    board = Board.update_empty_dot (Logic.position game.pacman) game.board;
  }

let process_meals game =
  if Board.got_big_dot (Logic.position game.pacman) game.board then
    eat_big_meal game
  else if Board.got_dot (Logic.position game.pacman) game.board then
    eat_meal game
  else game

let remaining_meals_rat game =
  let meals_left = Board.num_dots_left game.board in
  float_of_int meals_left /. float_of_int game.max_orbs

let diff_factor game fct = 1. -. (fct *. active_fraction game.difficulty)

let should_activate game fct =
  let remaining_meals = remaining_meals_rat game in
  let diff_factor = diff_factor game fct in
  remaining_meals < diff_factor

let activate_if_right game g =
  match Ghost.color g with
  | Red -> Ghost.activate g
  | Blue -> if should_activate game 1. then Ghost.activate g else g
  | Pink -> if should_activate game 2. then Ghost.activate g else g
  | Yellow -> if should_activate game 3. then Ghost.activate g else g

let activate_gs game =
  let gs' = game.ghosts |> List.map (activate_if_right game) in
  { game with ghosts = gs' }

let make_g_scatter diff g =
  if Ghost.scatter_frames g >= max_scatter diff then Ghost.unscatter g else g

let update_scatter_status game =
  {
    game with
    ghosts = game.ghosts |> List.map (make_g_scatter game.difficulty);
  }

let update_state game =
  match (Board.won game.board, game.lives) with
  | true, _ -> { game with game_state = Won }
  | false, 0 -> { game with game_state = Lost }
  | false, _ -> game

let moved_update game dir =
  game |> move_pac dir |> move_ghosts |> update_scatter_frames |> process_cols
  |> process_meals |> activate_gs |> update_scatter_status |> update_state

let update game input =
  match (game.paused, input) with
  | _, Command.Quit -> { game with quit_game = true }
  | true, Pause -> { game with paused = false }
  | true, _ -> game
  | false, Pause -> { game with paused = true }
  | false, Move dir -> moved_update game dir
  | false, Error s -> failwith s

let init_state board pac gs lives score paused quit_game game_state exits
    max_orbs difficulty =
  {
    board;
    pacman = pac;
    ghosts = gs;
    lives;
    score;
    paused;
    quit_game;
    game_state;
    exits;
    max_orbs;
    difficulty;
  }

let print_single = function
  | "C" -> ANSITerminal.print_string [ ANSITerminal.yellow ] "ᗧ "
  | "R" -> ANSITerminal.print_string [ ANSITerminal.red ] "ᗣ "
  | "B" -> ANSITerminal.print_string [ ANSITerminal.blue ] "ᗣ "
  | "P" -> ANSITerminal.print_string [ ANSITerminal.magenta ] "ᗣ "
  | "Y" -> ANSITerminal.print_string [ ANSITerminal.yellow ] "ᗣ "
  | "#" -> ANSITerminal.print_string [ ANSITerminal.blue ] "# "
  | "" -> ANSITerminal.print_string [ ANSITerminal.default ] "  "
  | "o" -> ANSITerminal.print_string [ ANSITerminal.default ] "▫ "
  | "|" -> ANSITerminal.print_string [ ANSITerminal.default ] "| "
  | "-" -> ANSITerminal.print_string [ ANSITerminal.default ] "--"
  | "_" -> ANSITerminal.print_string [ ANSITerminal.default ] "__"
  | x -> ANSITerminal.print_string [ ANSITerminal.default ] x

let print_row = Array.iter print_single

let seq r =
  print_row r;
  print_endline ""

let print_board arr = Array.iter seq arr
let edit_at_coord arr (x, y) s = arr.(y).(x) <- s
let ghost_rep g = if Ghost.is_scatter g then "👻" else "ᗣ "

let update_ghost_rep gs arr =
  gs
  |> List.map (fun g -> (Ghost.pos g, ghost_rep g))
  |> List.iter (fun (p, s) -> edit_at_coord arr p s)

let printable game =
  let arr = game.board in
  update_ghost_rep game.ghosts arr;
  arr

let blank () = Sys.command "clear" |> ignore
let welcome_message = "Hello! Welcome to Pacman!"

let print_game game =
  blank ();
  print_endline welcome_message;
  print_endline ("Current Lives " ^ string_of_int game.lives);
  print_endline ("Current Score " ^ string_of_int game.score);
  print_board (printable game);
  print_endline "Use the WASD keys to move Pacman";
  if game.paused then print_endline "Game Paused (p to resume)" else ()
