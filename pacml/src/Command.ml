type dir =
  | Up
  | Left
  | Down
  | Right

type command =
  | Move of dir
  | Pause
  | Quit
  | Error of string

exception Empty
exception Malformed

let parse str =
  match List.filter (fun x -> x <> "") (String.split_on_char ' ' str) with
  | [] -> raise Empty
  | [ "q" ] -> Quit
  | [ "p" ] -> Pause
  | [ "w" ] | [ "W" ] -> Move Up
  | [ "a" ] | [ "A" ] -> Move Left
  | [ "s" ] | [ "S" ] -> Move Down
  | [ "d" ] | [ "D" ] -> Move Right
  | _ -> Error "Key not recognized"
