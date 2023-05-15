open Command

type t = string array array
type position = int * int

let board_array t = Array.map Array.copy t
let border = "#"
let pac_dots = "O"
let big_pac_dots = "@"
let empty = ""
let container_col = "|"
let container_row = "-"
let exit_row = "_"

let is_border (t : t) (position : position) : bool =
  try
    let row = t.(snd position) in
    let cell = row.(fst position) in
    cell = border
  with Invalid_argument _ -> false

let is_container (t : t) (position : position) : bool =
  try
    let row = t.(snd position) in
    let cell = row.(fst position) in
    cell = container_col || cell = container_row
  with Invalid_argument _ -> false

let is_container_exit (t : t) (position : position) : bool =
  try
    let row = t.(snd position) in
    let cell = row.(fst position) in
    cell = exit_row
  with Invalid_argument _ -> false

let rec num_dots_row row =
  match row with
  | [] -> 0
  | h :: t ->
      if h = pac_dots || h = big_pac_dots then 1 + num_dots_row t
      else num_dots_row t

let rec num_dots_helper rows : int =
  match rows with
  | [] -> 0
  | row :: left_rows -> num_dots_row row + num_dots_helper left_rows

let num_dots_left (t : t) =
  let rows = Array.to_list (Array.map Array.to_list t) in
  num_dots_helper rows

let update_empty_dot (position : position) (t : t) =
  let dup = Array.map Array.copy t in
  match position with
  | x, y ->
      let new_pos = dup.(y).(x) in
      if new_pos = pac_dots || new_pos = big_pac_dots then (
        dup.(y).(x) <- empty;
        dup)
      else dup

let won t = if num_dots_left t = 0 then true else false

let got_dot position t =
  match position with
  | x, y -> t.(y).(x) = pac_dots

let got_big_dot position t =
  match position with
  | x, y -> t.(y).(x) = big_pac_dots

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
  | "R" -> "x"
  | "B" -> "x"
  | "Y" -> "x"
  | "P" -> "x"
  | "C" -> "c"
  | "O" -> "o"
  | _ -> c

let row_display row = Array.map char_display row
let board_display t = Array.map row_display t

let csv_array (board_csv : Csv.t) : string array array =
  let board_arr = Csv.to_array board_csv in
  Array.map
    (Array.map (fun s -> if s = "" then " " else String.make 1 s.[0]))
    board_arr
