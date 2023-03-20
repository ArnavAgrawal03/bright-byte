type t = {
  current: int*int;
 lives: int;
 cherries: (int*int) list;
 ghosts: (int*int) list;
 board: (string array) array;
 }

 let init_state (csv:Csv.t) : t = {
  current = (0,0);
  lives = 3;
  cherries = [(20,19); (-20, -19)];
  ghosts = [(10,10); (-3, -9)];
  board = Csv.to_array csv
 }

 let current (x:t) : int*int = x.current
 let lives (x:t) : int = x.lives
 let cherries (x:t) : (int*int) list = x.cherries
 let ghosts (x:t) : (int*int) list = x.ghosts
 let board (x:t) : (string list) list = Array.to_list (Array.map Array.to_list x.board)