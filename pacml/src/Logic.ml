type t = {
  position : Board.position;
  dir : Command.dir;
  og_pos : Board.position;
}

let pacman position dir og_pos = { position; dir; og_pos }
let char_rep = "C"
let position t = t.position
let dir t = t.dir
let og_pos t = t.og_pos

let move_pac_helper t board dir =
  let array_of_board = Board.board_array board in
  let board_height = Array.length array_of_board in
  let board_length = Array.length array_of_board.(snd t.position) in
  let new_pos = Board.move_pos t.position dir in
  if
    Board.is_border board new_pos
    || Board.is_container board new_pos
    || Board.is_container_exit board new_pos
  then t
  else
    let pos_x = (fst new_pos + board_length) mod board_length in
    let pos_y = (snd new_pos + board_height) mod board_height in
    let position = (pos_x, pos_y) in
    { t with position; dir }

let move_pac t board (dir : Command.dir) =
  let try_moving = move_pac_helper t board dir in
  if try_moving.position = t.position then
    let t_dir = t.dir in
    move_pac_helper t board t_dir
  else try_moving

let make_og_pos t = { t with position = t.og_pos }
