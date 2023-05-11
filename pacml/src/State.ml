open ANSITerminal
open Board

type current_game =
  | Won
  | Lost
  | Play

type difficulty =
  | Easy
  | Medium
  | Hard

type t = {
  current : int * int;
  lives : int;
  cherries : (int * int) list;
  ghosts : (int * int) list;
  board : string array array;
  total_pac_dots : int;
  score : int;
  is_paused : bool;
  quitting_game : bool;
  game_state : current_game;
}

let current (x : t) : int * int = x.current
let lives (x : t) : int = x.lives
let cherries (x : t) : (int * int) list = x.cherries
let ghosts (x : t) : (int * int) list = x.ghosts
let board (x : t) : string array array = x.board
let total_pac_dots (x : t) : int = x.total_pac_dots
let score (x : t) : int = x.score
let is_paused (x : t) : bool = x.is_paused
let quitting_game (x : t) : bool = x.quitting_game
let game_state (x : t) : current_game = x.game_state

let init_state (csv : Csv.t) =
  {
    current = (0, 0);
    lives = 3;
    cherries = [ (20, 19); (-20, -19) ];
    ghosts = [ (10, 10); (-3, -9) ];
    board = Csv.to_array csv;
    score = 0;
    is_paused = false;
    game_state = Play;
    quitting_game = false;
    total_pac_dots = Board.num_dots_left board;
  }

let frames_scat diff =
  match diff with
  | Easy -> 50
  | Medium -> 20
  | Hard -> 10

let pac_dot_value = 1
let ghost_value = 100
