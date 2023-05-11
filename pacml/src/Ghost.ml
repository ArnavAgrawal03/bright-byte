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

let init pos color dir =
  {
    pos;
    og_pos = pos;
    color;
    dir;
    active = true;
    scatter = false;
    scatter_frames = 0;
  }

(* querying functions*)
let pos t = t.pos
let color t = t.color
let dir t = t.dir
let active t = t.active

(* transforming functions*)
let move_aux dx dy pos = (fst pos + dx, snd pos + dy)

let move (g : t) (steps : int) (dir : Command.dir) : t =
  match dir with
  | Up -> { g with pos = move_aux 0 (-steps) g.pos }
  | Down -> { g with pos = move_aux 0 steps g.pos }
  | Right -> { g with pos = move_aux steps 0 g.pos }
  | Left -> { g with pos = move_aux (-steps) 0 g.pos }
