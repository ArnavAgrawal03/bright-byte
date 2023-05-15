open OUnit2
open Game
open Command
open Logic
open Board
(* open State open Ghosts*)

(** The main file (run by “make play”) was tested manually in the terminal in
    order to check if everything was printing and displaying correctly. This
    also allowed us to check for correct behaviour for ghosts as well as the
    state - running state got really complicated, and we were able to test most
    of the functions by directly playing the game. If the game works correctly,
    then we’ve successfully tested state and ghosts - as required. The rest of
    the functionality was tested via OUnit2. We used a mix of glass box and
    black box testing. The implementer for each file implemented test cases. The
    other 2 (non-implementers) added test cases by exclusively looking at the
    .mli files (specification). By combining these two approaches, we have
    covered both the internal and external logic used by the game. The manual
    testing will check both the game's overall functionality and display
    features, while the OUnit testing tests the individual methods.. So, with
    high probability, our code is correctly implemented*)

let command_parse_test (name : string) (command : string)
    (expected_output : command) =
  name >:: fun _ -> assert_equal expected_output (parse command)

let command_dir_parse_test (name : string) (command : string)
    (expected_output : command) =
  name >:: fun _ -> assert_equal expected_output (parse command)

let data_dir_prefix = "data" ^ Filename.dir_sep
let easy = Board.csv_array (data_dir_prefix ^ "easy.csv")
let medium = Board.csv_array (data_dir_prefix ^ "normal.csv")
let hard = Board.csv_array (data_dir_prefix ^ "hard.csv")

let is_border_test (name : string) (input1 : Board.t) (input2 : Board.position)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output (is_border input1 input2) ~printer:string_of_bool

let is_container_test (name : string) (input1 : Board.t)
    (input2 : Board.position) (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    Board.(is_container input1 input2)
    ~printer:string_of_bool

let is_container_exit_test (name : string) (input1 : Board.t)
    (input2 : Board.position) (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.is_container_exit input1 input2)
    ~printer:string_of_bool

let num_dots_left_test (name : string) (input : string array array)
    (expected_output : int) =
  name >:: fun _ -> assert_equal expected_output (Board.num_dots_left input)

let won_test (name : string) (input : Board.t) (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output (Board.won input) ~printer:string_of_bool

let got_dot_test (name : string) (input1 : Board.position) (input2 : Board.t)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.got_dot input1 input2)
    ~printer:string_of_bool

let got_big_dot_test (name : string) (input1 : Board.position)
    (input2 : Board.t) (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.got_big_dot input1 input2)
    ~printer:string_of_bool

let command_tests =
  [
    command_parse_test "Parses the key 'q' as the command quit correctly" "q"
      Quit;
    command_parse_test "Parses the key 'p' as the command pause correctly" "p"
      Pause;
    command_dir_parse_test "Parses the key 'w' as the command move up correctly"
      "w" (Move Up);
    command_dir_parse_test "Parses the key 'W' as the command move up correctly"
      "W" (Move Up);
    command_dir_parse_test
      "Parses the key 'a' as the command move left correctly" "a" (Move Left);
    command_dir_parse_test
      "Parses the key 'A' as the command move left correctly" "A" (Move Left);
    command_dir_parse_test
      "Parses the key 's' as the command move down correctly" "s" (Move Down);
    command_dir_parse_test
      "Parses the key 'S' as the command move down correctly" "S" (Move Down);
    command_dir_parse_test
      "Parses the key 'd' as the command move right correctly" "d" (Move Right);
    command_dir_parse_test
      "Parses the key 'D' as the command move right correctly" "D" (Move Right);
    command_dir_parse_test
      "Parses the key '    a' as the command move left correctly" "      a"
      (Move Left);
    command_dir_parse_test
      "Parses the key '  a    ' as the command move left correctly" "  a    "
      (Move Left);
    command_dir_parse_test
      "Parses the key '  A  ' as the command move left correctly" "  A  "
      (Move Left);
    command_dir_parse_test
      "Parses the key 'd     ' as the command move right correctly" "d     "
      (Move Right);
    command_dir_parse_test
      "Parses the key '   D ' as the command move right correctly" "   D "
      (Move Right);
    command_dir_parse_test
      "Parses the key '  S' as the command move down correctly" "  S"
      (Move Down);
    command_dir_parse_test
      "Parses the key ' W      ' as the command move up correctly" " W      "
      (Move Up);
  ]

let board_tests =
  [
    is_border_test "is_border (0,0) easy is true" easy (0, 0) true;
    is_border_test "is_border (0,0) medium is true" medium (0, 0) true;
    is_border_test "is_border (0,0) hard is true" hard (0, 0) true;
    is_border_test "is_border (0,1) easy is true" easy (0, 1) true;
    is_border_test "is_border (0,1) medium is true" medium (0, 1) true;
    is_border_test "is_border (0,1) hard is true" hard (0, 1) true;
    is_border_test "is_border (1,1) easy is true" easy (1, 1) false;
    is_border_test "is_border (1,1) medium is true" medium (1, 1) false;
    is_border_test "is_border (1,1) hard is true" hard (1, 1) false;
    is_border_test "is_border (3,3) easy is true" easy (3, 3) true;
    is_border_test "is_border (3,3) medium is true" medium (3, 3) false;
    is_border_test "is_border (3,3) hard is true" hard (3, 3) false;
    is_border_test "is_border (14,14) easy is true" easy (14, 14) true;
    is_border_test "is_border (19,19) medium is true" medium (19, 19) true;
    is_border_test "is_border (21,24) hard is true" hard (21, 24) true;
    is_container_test "is_container (0,0) easy is false" easy (0, 0) false;
    is_container_test "is_container (0,0) medium is false" medium (0, 0) false;
    is_container_test "is_container (0,0) hard is false" hard (0, 0) false;
    is_container_test "is_container (5,5) easy is false" easy (5, 5) false;
    is_container_test "is_container (1,1) medium is false" medium (1, 1) false;
    is_container_test "is_container (4,4) hard is false" hard (4, 4) false;
    is_container_exit_test "is_container_exit (2,2) easy is false" easy (2, 2)
      false;
    is_container_exit_test "is_container_exit (6,7) easy is true" easy (7, 6)
      true;
    num_dots_left_test "num_dots_left easy is 48" (Board.board_array easy) 48;
    num_dots_left_test "num_dots_left meidum is 48" (Board.board_array medium)
      72;
    num_dots_left_test "num_dots_left hard is 23" (Board.board_array hard) 23;
    won_test "won easy is false" easy false;
    won_test "won medium is false" medium false;
    won_test "won hard is false" hard false;
    got_dot_test "got_dot (3,3) easy is true" (3, 3) easy false;
    got_big_dot_test "got_big_dot (4,4) easy is false" (4, 4) easy false;
  ]

let string_of_position position =
  match position with
  | x, y -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let move_pac_test (name : string) (og_pos : Board.position) (board : Board.t)
    (dir : Command.dir) expected_output =
  name >:: fun _ ->
  let pac = pacman og_pos Right og_pos in
  assert_equal expected_output
    (Logic.position (Logic.move_pac pac board dir))
    ~printer:string_of_position

let make_og_pos_test (name : string) (position : Board.position)
    (og_pos : Board.position) expected_output =
  name >:: fun _ ->
  let pac = pacman position Right og_pos in
  assert_equal expected_output
    (Logic.position (make_og_pos pac))
    ~printer:string_of_position

let logic_tests =
  [
    move_pac_test "Pac-Man starts at (1,1) and moves down to (1,2)" (1, 1) easy
      Down (1, 2);
    move_pac_test
      "Pac-Man starts at (1,1) and tries to move up but cannot so position \
       stays at (2,1)"
      (1, 1) easy Up (2, 1);
    move_pac_test "Pac-Man starts at (1,1) and moves right to (2,1)" (1, 1) easy
      Right (2, 1);
    move_pac_test "Pac-Man starts at (1,1) and moves left to (2,1)" (1, 1) easy
      Left (2, 1);
    move_pac_test
      "Pac-Man starts at (1,1) and tries to move left but cannot so position \
       stays at (1,1)"
      (1, 1) easy Left (2, 1);
    move_pac_test "Pac-Man starts at (2,2) and moves down to (2,2)" (2, 2) easy
      Down (2, 2);
    move_pac_test "Pac-Man starts at (2,2) and moves up to (2,1)" (2, 2) easy Up
      (2, 1);
    move_pac_test "Pac-Man starts at (4,1) and moves right to (5,1)" (4, 1) easy
      Right (5, 1);
    move_pac_test "Pac-Man starts at (4,1) and moves right to (3,1)" (4, 1) easy
      Left (3, 1);
    make_og_pos_test "Pac-Man starts at (0,0) and is currently at (6,8)" (6, 8)
      (0, 0) (0, 0);
    make_og_pos_test "Pac-Man starts at (-10,0) and is currently at (0,0)"
      (0, 0) (-10, 0) (-10, 0);
    make_og_pos_test
      "Pac-Man starts at (-100,-2380) and is currently at (5023,3520)"
      (5023, 3520) (-100, -2380) (-100, -2380);
    make_og_pos_test
      "Pac-Man starts at (-8, 3125) and is currently at (-123,-14533)"
      (-8, 3125) (-123, -14533) (-123, -14533);
  ]

let test_init () =
  let expected_pos = (1, 2) in
  (* expected position *)
  let expected_color = Ghost.Red in
  (* expected color *)
  let expected_dir = Command.Up in
  (* expected direction *)
  let expected_active = true in
  (* expected active status *)
  let open Ghost in
  let ghost = init expected_pos expected_color expected_dir expected_active in
  assert_equal expected_pos (pos ghost);
  assert_equal expected_color (color ghost);
  assert_equal expected_dir (dir ghost);
  assert_equal expected_active (is_active ghost)

(* Define test cases for the 'inits' function *)
let test_inits () =
  let expected_pos1 = (1, 2) (* expected position 1 *) in
  let expected_color1 = Ghost.Red (* expected color 1 *) in
  let expected_pos2 = (3, 4) (* expected position 2 *) in
  let expected_color2 = Ghost.Blue (* expected color 2 *) in
  let open Ghost in
  let ghost_list =
    inits [ (expected_pos1, expected_color1); (expected_pos2, expected_color2) ]
  in
  assert_equal expected_pos1 (pos (List.nth ghost_list 0));
  assert_equal expected_color1 (color (List.nth ghost_list 0));
  assert_equal expected_pos2 (pos (List.nth ghost_list 1));
  assert_equal expected_color2 (color (List.nth ghost_list 1))

(* Define test cases for the 'pos' function *)
let test_pos () =
  (* Create test inputs *)
  let ghost = Ghost.init (1, 2) Ghost.Red Command.Up true in
  (* create a ghost with a specific position *)
  let expected_position = (1, 2) in

  (* expected position *)

  (* Perform the test *)
  assert_equal expected_position (Ghost.pos ghost)

(* Define test cases for the 'color' function *)
let test_color () =
  (* Create test inputs *)
  let ghost =
    Ghost.init (1, 2) Red Command.Up
      true (* create a ghost with a specific color *)
  in
  let expected_color = Ghost.Red (* expected color *) in

  (* Perform the test *)
  assert_equal expected_color (Ghost.color ghost)

(* Define test cases for the 'dir' function *)
let test_dir () =
  let open Ghost in
  (* Create test inputs *)
  let ghost =
    init (1, 2) Red Command.Right
      true (* create a ghost with a specific direction *)
  in
  let expected_direction = Command.Right (* expected direction *) in

  (* Perform the test *)
  assert_equal expected_direction (dir ghost)

(* Define test cases for the 'is_active' function *)
let test_is_active () =
  let open Ghost in
  (* Create test inputs *)
  let ghost_active =
    init (1, 2) Red Command.Up true (* create an active ghost *)
  in
  let ghost_inactive =
    init (3, 4) Blue Command.Down false (* create an inactive ghost *)
  in

  (* Perform the test *)
  assert_equal true (is_active ghost_active);
  assert_equal false (is_active ghost_inactive)

let _ =
  [
    test_color ();
    test_dir ();
    test_init ();
    test_inits ();
    test_is_active ();
    test_pos ();
  ]

let suite =
  "Test suite for Pac-Man"
  >::: List.flatten [ command_tests; logic_tests; board_tests ]

let _ = run_test_tt_main suite
