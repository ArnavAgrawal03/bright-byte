open Game
open Command

(*open Logic*)
open State

let data_dir_prefix = "data" ^ Filename.dir_sep
let clear () = Sys.command "clear"

type t =
  | A of Csv.t
  | B of string

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

(*let single_char () = Unix.tcsetattr Unix.stdin TCSANOW { (Unix.tcgetattr
  Unix.stdin) with c_icanon = false }*)

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

let move g = function
  | Up ->
      print_string "Moving up! \n";
      g
  | Left ->
      print_string "Moving left! \n";
      g
  | Right ->
      print_string "Moving right! \n";
      g
  | Down ->
      print_string "Moving down! \n";
      g

(**[play_game g] prints out the board g and parses parses through the user's
   command*)
let rec play_game g : unit =
  let _ = clear () in
  print_board (Array.to_list (Array.map Array.to_list (board g)));
  match read_line () |> parse with
  | Move m -> play_game (move g m)
  | Pause ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "Game paused. Press p to resume. Press q to quit. \n";
      play_game g
  | Quit -> exit 0
  | Error a ->
      ANSITerminal.print_string [ ANSITerminal.red ] (a ^ "!\n");
      play_game g

(**[start_game filename] retrives the file and starts the game in the initial
   state of g *)
let start_game filename : unit =
  let file = get_file filename in
  match file with
  | A g -> play_game (init_state g)
  | B m -> print_string m

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let _ = clear () in
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Pacman!\n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Please choose the difficulty (easy, medium, hard): \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> start_game (data_dir_prefix ^ file_name ^ ".csv")

(* Execute the game engine. *)
let () = main ()
