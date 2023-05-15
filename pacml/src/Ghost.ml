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

let directions =
  let open Command in
  [ Up; Down; Left; Right ]

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

let start_pos g = g.og_pos
let reset g = { g with pos = g.og_pos }
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
let activate g = if is_active g then g else switch_active g
let deactivate g = if not (is_active g) then g else switch_active g
let switch_scatter g = { g with scatter = not g.scatter; scatter_frames = 0 }
let scatter g = if is_scatter g then g else switch_scatter g
let unscatter g = if not (is_scatter g) then g else switch_scatter g
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

let rec move_n_steps pos dir = function
  | 0 -> pos
  | steps ->
      let pos' = Board.move_pos pos dir in
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
  else scatter_target board Yellow

let find_by_color c lst =
  let filtered = List.filter (fun x -> color x = c) lst in
  assert (List.length filtered = 1);
  List.hd filtered

let get_red = find_by_color Red
let get_blue = find_by_color Blue
let get_yellow = find_by_color Yellow
let get_pink = find_by_color Pink

let chase_target g pac gs board =
  match color g with
  | Red -> red_target pac
  | Blue -> blue_target pac (get_red gs)
  | Pink -> pink_target pac
  | Yellow -> yellow_target g pac board 8

let target g pac gs board =
  if is_scatter g then scatter_target board (color g)
  else chase_target g pac gs board

let move_one_step p (d : Command.dir) =
  let step =
    match d with
    | Up -> (0, -1)
    | Down -> (0, 1)
    | Left -> (-1, 0)
    | Right -> (1, 0)
  in
  point_sum p step

let randint max_val =
  (* initialize random environment to ensure a highly random seed*)
  Random.self_init ();
  Random.int max_val

(* compare the distance from target of two posible position options*)
let distance_cmp x y =
  match Stdlib.compare x y with
  | 1 -> 1
  | -1 -> -1
  | _ ->
      let choices = [ -1; 1 ] in
      List.nth choices (randint 2)

(* compare function used for sorting through a list of ghosts*)
let cmp p1 p2 = distance_cmp (snd p1) (snd p2)

let move_options g target =
  let position = pos g in
  let possible_positions = List.map (move_one_step position) directions in
  let dists_from_target = List.map (distance_man target) possible_positions in
  let unsorted_options = List.combine directions dists_from_target in
  List.sort cmp unsorted_options

(* let string_of_coord (x, y) = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^
   ")" *)

let step_aux g board dir is_blocked =
  let arr = Board.board_array board in
  let length = Array.length board in
  let breadth = Array.length arr.(1) in
  let step_target = Board.move_pos (pos g) dir in
  (* let _ = print_endline (string_of_coord step_target) in let _ =
     print_endline (string_of_bool (is_blocked step_target)) in *)
  match is_blocked step_target with
  | true -> g
  | false ->
      let wrap_around a b = (a + b) mod length in
      let delta = (breadth, length) in
      let pos' = point_op delta step_target wrap_around in
      { g with pos = pos'; dir }

let rec dequeue (target_dir : Command.dir) lst =
  match lst with
  | [] -> []
  | (dir', dist') :: tail when dir' = target_dir -> tail @ [ (dir', dist') ]
  | h :: tail -> h :: dequeue target_dir tail

let rec move_best_option options board g is_blocked =
  match options with
  | [] -> g (* no options available!*)
  | data :: rest_options ->
      let g' = step_aux g board (fst data) is_blocked in
      if pos g' <> pos g then g' (* sucessfully moved positions*)
      else
        move_best_option rest_options board g
          is_blocked (* somehow blocked, so try next best option*)

(* check is move blocked by container or border - written in a way to add
   additional checks for future features*)
let blocked_aux func_list board position =
  let f g = g board position in
  let bool_vals = List.map f func_list in
  List.fold_left ( || ) false bool_vals

let wall_border = [ Board.is_border ]

let all_borders =
  let open Board in
  [ is_border; is_container; is_container_exit ]

let non_exit_borders = Board.[ is_border; is_container ]
let blocked_all = blocked_aux all_borders
let blocked_wall = blocked_aux wall_border
let blocked_non_exit = blocked_aux non_exit_borders

let reorder options (dir : Command.dir) =
  match dir with
  | Up -> dequeue Down options
  | Down -> dequeue Up options
  | Right -> dequeue Left options
  | Left -> dequeue Right options

let move_unlocked pac board gs g =
  let targeted = target g pac gs board in
  let options = move_options g targeted in
  let directed_options =
    reorder options (dir g)
    (* going reverse must be the last option irrespective of direction*)
  in
  move_best_option directed_options board g (blocked_all board)

let move_locked board g exits =
  let poss_moves = List.flatten (List.map (move_options g) exits) in
  let ordered_moves = dequeue (dir g) poss_moves in
  move_best_option ordered_moves board g (blocked_wall board)

let move_exit board g =
  let options = move_options g (0, 0) in
  move_best_option options board g (blocked_non_exit board)

let move_all pac board gs g exits =
  match
    (Board.is_container board (pos g), Board.is_container_exit board (pos g))
  with
  | true, _ -> move_locked board g exits
  | _, true -> move_exit board g
  | _ -> move_unlocked pac board gs g

let random_trajectories () =
  let options = List.map (fun x -> (x, randint 100)) directions in
  List.sort cmp options

let blocked_inactive board position = not (Board.is_container board position)

let move_all_locked pac board gs g exits =
  if blocked_inactive board (pos g) then
    let g' = { g with active = true } in
    move_all pac board gs g' exits
  else
    move_best_option (random_trajectories ()) board g (blocked_inactive board)

let move pac board gs exits g =
  if is_active g then move_all pac board gs g exits
  else move_all_locked pac board gs g exits

let rec random_directions g board l =
  Command.(
    match l with
    | [] -> Up
    | (dir, _) :: t ->
        let g' = step_aux g board dir (blocked_wall board) in
        if pos g' <> pos g then dir else random_directions g board t)

let init_random arg_list board =
  let create_ghost (pos', color') =
    {
      og_pos = pos';
      pos = pos';
      color = color';
      active = false;
      scatter = false;
      scatter_frames = 0;
      dir = Up;
    }
  in
  (let get_dir g board = random_directions g board (random_trajectories ()) in
   List.map
     (fun (pos_a, color_a) ->
       init pos_a color_a (get_dir (create_ghost (pos_a, color_a)) board) false)
     arg_list
    : t list)

let incr_scatter_frames g = { g with scatter_frames = g.scatter_frames + 1 }
