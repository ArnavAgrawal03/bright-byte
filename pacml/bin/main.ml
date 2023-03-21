open Game
open Command
(*open Logic*)
open State

let data_dir_prefix = "data" ^ Filename.dir_sep

type t =
  | A of Csv.t
  | B of string

let get_file f =
  try A (Csv.load f)
  with _ -> B ("There is no file with path: " ^ f)
  
let rec join_list = function
  | [] -> ""
  | [ s ] -> s
  | s :: tl -> s ^ " " ^ join_list tl

(*let rec print_line = function
| [] -> print_string ""
| hd :: tl -> print_string hd; print_line tl
*)

let rec print_board (b: string list list) = match b with
| [] -> print_string "\n"
| hd :: tl -> print_string (join_list hd); print_string "\n"; print_board tl
let play_game g :unit = 
  print_board (board g);
  match read_line () |> parse with
  | Move _ -> print_string "Moving pacman"
  | Pause -> print_string "Game paused"
  | Quit -> exit 0
let start_game filename : unit = let file = get_file filename in 
  match file with
  | A g -> play_game (init_state g)
  | B m -> print_string m

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
    ANSITerminal.print_string [ ANSITerminal.red ] ("\n\nWelcome to Pacman!\n");
    ANSITerminal.print_string [ ANSITerminal.blue ] "Please choose the difficulty (easy, medium, hard): \n";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | file_name -> start_game (data_dir_prefix ^ file_name ^ ".csv")

(* Execute the game engine. *)
let () = main ()