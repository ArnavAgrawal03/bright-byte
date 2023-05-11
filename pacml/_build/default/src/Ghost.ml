type color =
  | Red
  | Blue
  | Pink
  | Yellow

type t = {
  pos : Board.position;
  start_pos : Baord.position;
  dir : Command.move;
  color : color;
  active : bool;
  scatter : bool;
  scatter_frames : int;
}
