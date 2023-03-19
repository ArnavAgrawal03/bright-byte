type object_phrase = string list

type command = 
| Name of object_phrase
| Level of int
| Pause
| Quit

exception Empty
exception Malformed

let parse str =
  match List.filter (fun x -> x <> "") (String.split_on_char ' ' str) with
  | [] -> raise Empty
  | [ "q" ] -> Quit
  | [ "p" ] -> Pause
  | "Name" :: tl -> Name tl
  | "Level" :: [x] -> Level (x|> int_of_string)
  | _ -> raise Malformed