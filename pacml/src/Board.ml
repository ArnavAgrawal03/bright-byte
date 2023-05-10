open Command

type t = char array array
type position = int * int

(* need to change ghost wall and exit*)
let board_array t = Array.map Array.copy t
let border = "#"
let pac_dots = "O"
let empty = ""

let is_boarder t position =
  try t.(snd position).(fst position) = border
  with Invalid_argument arg -> false

let rec num_dots_row row =
  match row with
  | [] -> 0
  | h :: t -> if h = pac_dots then 1 + num_dots_row t else num_dots_row t

let rec num_dots_helper rows : int =
  match rows with
  | [] -> 0
  | row :: left_rows -> num_dots_row row + num_dots_helper left_rows

let rec num_dots_left t =
  let rows = Array.to_list (Array.map Array.to_list t) in
  num_dots_helper rows

let update_empty_dot position t =
  let dup = Array.map Array.copy t in
  match position with
  | x, y ->
      let new_pos = dup.(y).(x) in
      if new_pos = pac_dots then (
        dup.(y).(x) <- empty;
        dup)
      else dup

let won t = if num_dots_left t = 0 then true else false

let got_dot postion t =
  match postion with
  | x, y -> t.(y).(x) = pac_dots

let helper_move_pos change_x change_y position =
  match position with
  | x, y -> (x + change_x, y + change_y)

let move_pos position (dir : Command.dir) =
  match dir with
  | Up -> helper_move_pos 0 (-1) position
  | Left -> helper_move_pos (-1) 0 position
  | Right -> helper_move_pos 1 0 position
  | Down -> helper_move_pos 0 1 position

let char_display c =
  match c with
  | 'R' -> 'x'
  | 'B' -> 'x'
  | 'Y' -> 'x'
  | 'P' -> 'x'
  | 'C' -> 'c'
  | '0' -> 'o'
  | _ -> c

let row_display row = Array.map char_display row
let board_display t = Array.map row_display t
