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

let pos_of t = t.pos
let color_of t = t.color
let move t pos = { t with pos }
