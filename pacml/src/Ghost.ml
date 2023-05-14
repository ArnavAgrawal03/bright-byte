type color =
  | Red
  | Blue
  | Pink
  | Yellow

type t = {
  pos : Board.position;
  og_pos : Board.position;
  color : color;
  dir : Command.dir;
  active : bool;
  scatter : bool;
  scatter_frames : int;
}

(*-------- Constructor --------*)
let init pos_a color_a dir_a active_a =
  {
    pos = pos_a;
    og_pos = pos_a;
    color = color_a;
    dir = dir_a;
    active = active_a;
    scatter = false;
    scatter_frames = 0;
  }

let inits arg_list =
  List.map (fun (pos_a, color_a) -> init pos_a color_a Up false) arg_list

(*-------- Querying functions --------*)

(* querying functions*)
let pos g = g.pos
let color g = g.color
let dir g = g.dir
let is_active g = g.active
let is_scatter g = g.scatter
let scatter_frames g = g.scatter_frames

let scatter_target m c =
  (* 4 ghosts go to the 4 corners of the board*)
  let arr = Board.board_array m in
  let hor_end = Array.length arr.(1) - 1 in
  let ver_end = Array.length arr - 1 in
  match c with
  | Red -> (0, 0)
  | Blue -> (hor_end, 0)
  | Pink -> (0, ver_end)
  | Yellow -> (hor_end, ver_end)

(*-------- Tranforming functions --------*)

let turn g d = { g with dir = d }

let rev g =
  let dir' =
    match g.dir with
    | Up -> Command.Down
    | Down -> Command.Up
    | Left -> Command.Right
    | Right -> Command.Left
  in
  turn g dir'

let switch_active g = { g with active = not g.active }

let activate g =
  assert (not g.active);
  switch_active g

let deactivate g =
  assert g.active;
  switch_active g

let switch_scatter g = { g with scatter = not g.scatter; scatter_frames = 0 }

let scatter g =
  assert (not g.scatter);
  switch_scatter g

let unscatter g =
  assert g.scatter;
  switch_scatter g

let l1_norm p = float_of_int (abs (fst p) + abs (snd p))
let l2_norm p = sqrt (float_of_int ((fst p * fst p) + (snd p * snd p)))
let point_op p1 p2 op = (op (fst p1) (fst p2), op (snd p1) (snd p2))
let point_difference p1 p2 = point_op p1 p2 ( - )
let point_sum p1 p2 = point_op p1 p2 ( + )

let distance p1 p2 man =
  let diff = point_difference p1 p2 in
  match man with
  | true -> l1_norm diff
  | false -> l2_norm diff

let distance_man p1 p2 = distance p1 p2 true
let distance_euc p1 p2 = distance p1 p2 false

let move_one_step p (d : Command.dir) =
  match d with
  | Up -> (fst p, snd p - 1)
  | Down -> (fst p, snd p + 1)
  | Left -> (fst p - 1, snd p)
  | Right -> (fst p + 1, snd p)

let rec move_n_steps pos dir = function
  | 0 -> pos
  | steps ->
      let pos' = move_one_step pos dir in
      move_n_steps pos' dir (steps - 1)

let red_target pac = Logic.position pac

let blue_target pac red =
  let pac_position = Logic.position pac in
  let pac_direction = Logic.dir pac in
  let red_position = pos red in
  let intercept_ahead = move_n_steps pac_position pac_direction 2 in
  let red_lag = point_difference intercept_ahead red_position in
  point_sum intercept_ahead red_lag

let pink_target pac =
  let pac_position = Logic.position pac in
  let pac_direction = Logic.dir pac in
  let intercept_ahead = move_n_steps pac_position pac_direction 4 in
  intercept_ahead

let yellow_target g pac board threshold =
  let pac_position = Logic.position pac in
  let pac_direction = Logic.dir pac in
  if distance_man pac_position (pos g) <= float_of_int threshold then
    let intercept_ahead = move_n_steps pac_position pac_direction threshold in
    intercept_ahead
  else scatter_target board (color g)
