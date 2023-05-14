open Csv
open Game
open Command
open Board
open State

exception Timeout
exception Invalid_argument

type t =
  | A of Csv.t
  | B of string

let data_dir_prefix = "data" ^ Filename.dir_sep
let clear () = ignore (Sys.command "clear")
let frame_time = 1
let default_terminal = Unix.tcgetattr Unix.stdin

let go_to_default_terminal () =
  Unix.tcsetattr Unix.stdin TCSANOW default_terminal

let single_char () =
  Unix.tcsetattr Unix.stdin TCSANOW
    { (Unix.tcgetattr Unix.stdin) with c_icanon = false }

let timeout f x max_time =
  let handle_sigalrm signal =
    if signal = Sys.sigalrm then raise Timeout else ()
  in
  let set_signal_handle =
    Sys.set_signal Sys.sigalrm (Signal_handle handle_sigalrm)
  in
  let _ = ignore (Unix.alarm max_time) in
  let value = f x in
  let _ = ignore (Unix.alarm 0) in
  value

(**[get_file f] attempts to retrive csv f*)
let get_file f =
  try A (Csv.load f) with _ -> B ("There is no file with path: " ^ f)

(**[join_list] is a helper function that concates the cells in the csv to the
   set of the row in order to print the board*)
let rec join_list = function
  | [] -> ""
  | [ s ] -> s
  | s :: tl ->
      if s <> "" then s ^ " " ^ join_list tl else s ^ " " ^ join_list tl

(** [single_char] changes the terminal settings to allow parsing of a single
    character. *)

let rec print_line = function
  | [] -> print_string " "
  | [ "/n" ] -> print_string "\n"
  | "#" :: tl ->
      ANSITerminal.print_string [ ANSITerminal.blue ] "# ";
      print_line tl
  | "O" :: tl ->
      ANSITerminal.print_string [ ANSITerminal.green ] "▫ ";
      print_line tl
  | "R" :: tl ->
      ANSITerminal.print_string [ ANSITerminal.red ] "👻";
      print_line tl
  | "B" :: tl ->
      ANSITerminal.print_string [ ANSITerminal.cyan ] "👻";
      print_line tl
  | "P" :: tl ->
      ANSITerminal.print_string [ ANSITerminal.magenta ] "👻";
      print_line tl
  | "Y" :: tl ->
      ANSITerminal.print_string [ ANSITerminal.white ] "👻";
      print_line tl
  | "C" :: tl ->
      ANSITerminal.print_string [ ANSITerminal.yellow ] "ᗧ ";
      print_line tl
  | "" :: tl ->
      print_string "  ";
      print_line tl
  | x -> print_string (join_list x)

(** [print_board] prints the current board on the terminal*)
let rec print_board (b : string list list) =
  match b with
  | [] -> print_string "\n"
  | hd :: tl ->
      print_line hd;
      (*print_string "\n";*)
      print_board tl

let player_input () =
  single_char ();
  let command =
    let single_input = input_char stdin in
    match single_input with
    | 'w' -> Some (Move Up)
    | 'a' -> Some (Move Left)
    | 's' -> Some (Move Down)
    | 'd' -> Some (Move Right)
    | 'p' -> Some Pause
    | 'q' -> Some Quit
    | _ -> raise Invalid_argument
  in
  go_to_default_terminal ();
  command

(**[start_game filename] retrives the file and starts the game in the initial
   state of g *)
let update_state_ref state_ref cmd_ref command = cmd_ref := match command
   with | Some (Pause | Quit) | None -> !cmd_ref | Some (Move m) -> Move m | _ -> raise Invalid_argument;

   state_ref := match command with | Some cmd -> State.update !state_ref cmd |
   None -> State.update !state_ref !cmd_ref

   let rec handle_playing_state state command = update_state_ref state command
   (try timeout player_input () frame_time with Timeout -> None); State.print
   !state; if State.game_state !state = Won then begin print_endline
   "Congratulations! You won the game!"; state := State.update !state Quit; end
   else if State.game_state !state = Lost then begin print_endline "Oops! You
   lost the game :("; state := State.update !state Quit; end else
   handle_playing_state state command

   let rec playing_game (file : Csv.t) = let state = ref (State.init_state file) in let command = ref (Move
   Right) in State.print !state; handle_playing_state state command;
   print_endline "Thanks for playing! Press Enter to return to the main menu.";
   ignore (read_line ());

let start_game filename : unit =
  let file = Csv.load filename in
  match file with
  | g -> playing_game g
  | _ -> print_string "Invalid CSV format"

(*Do not touch below this!! it is all good*)

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  main_menu ();
  single_char ();
  let input = input_char stdin in
  go_to_default_terminal ();
  match input with
  | '1' -> directions ()
  | '2' -> play_game ()
  | '3' -> clear ()
  | _ -> main ()

and main_menu () =
  clear ();
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Pacman!\n";
  print_endline
    "What would you like to do?\n\
    \ 1. View the directions\n\
    \ 2. Play the game\n\
    \ 3. Quit"

and directions () =
  clear ();
  print_endline "To start the game:";
  print_endline "Press 2 when you are on the screen with the main menu.";
  print_endline
    "Then you will be prompted to choose a difficulty level easy, medium, hard.";
  print_endline
    "Type out the difficulty level you want and press enter and then the game \
     will start!";
  print_endline "";
  print_endline "How the board works:";
  print_endline "The yellow ᗧ represents your Pac-Man character.";
  print_endline
    "In order to move Pac-MAN use the WASD keys (up, left, down, right \
     respectively)";
  print_endline
    "The ghost emojis represent the ghosts and the green dots (▫) are the \
     pac-dots ";
  print_endline "The blue '#' respresent the walls.";
  print_endline "";
  print_endline "Rules of the game:";
  print_endline
    "The goal is to win the game by moving Pac-Man around the board until you \
     have collected all of the pac-dots on the board.";
  print_endline
    "If you run into a ghost, then you lose a life and your Pac-Man will \
     restart at it's original position";
  print_endline "You can press 'p' to pause/resume the game";
  print_endline "You can press 'q' to quit the game";
  print_endline
    "Additionally, we recommend making your terminal full screen for the best \
     playing experience!";
  print_endline "Thank you for playing and good luck!"

and play_game () : unit =
  let _ = clear () in
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Pacman!\n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Please choose the difficulty (easy, medium, hard): \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name ->
      let full_file_name = data_dir_prefix ^ file_name ^ ".csv" in
      start_game full_file_name

(* Execute the game engine. *)
let () = main ()
