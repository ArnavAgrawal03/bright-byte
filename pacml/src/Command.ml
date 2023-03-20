type object_phrase = string list

type dir = Up | Left | Down | Right

type command = 
| Name of object_phrase
| Level of int
| Move of dir
| Pause
| Quit

exception Empty
exception Malformed

let parse str =
  match List.filter (fun x -> x <> "") (String.split_on_char ' ' str) with
  | [] -> raise Empty
  | [ "q" ] -> Quit
  | [ "p" ] -> Pause
  | ["w"] | ["W"] -> Move Up
  | ["a"] | ["A"] -> Move Left
  | ["s"] | ["S"] -> Move Down
  | ["d"] | ["D"] -> Move Right
  | "Name" :: tl -> Name tl
  | "Level" :: [x] -> Level (x|> int_of_string)
  | _ -> raise Malformed