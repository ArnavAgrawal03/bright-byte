(*open Game
open Command
open Logic
open State*)

let data_dir_prefix = "data" ^ Filename.dir_sep

(*type t =
  | A of Csv.t
  | B of string

  let get_file f =
    try A (Csv.load f)
    with _ -> B ("There is no file with path: " ^ f)
  
  let rec join_list = function
    | [] -> ""
    | [ s ] -> s
    | s :: tl -> s ^ " " ^ join_list tl
*)
let play_game filename = print_string ("Looking for file " ^ filename)

  let main () =
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
      print_endline "Please enter the name of the game file you want to load.\n";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json")
    
    (* Execute the game engine. *)
    let () = main ()