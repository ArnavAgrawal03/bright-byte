open OUnit2
open Game
open Command
open Logic
open Board
(* open State open Ghosts*)

(** The main file was tested manually in the terminal in order to check if
    everything was printing and displaying correctly. The rest of the
    functionality was automatically testing by OUnit. For this testing suite, we
    used a glass box strategy. This was because we wanted to find the edge cases
    inside the methods in order to see if they were implemented correctly. By
    combining these two approaches, we have covered both the internal and
    external logic used by the game. The manual testing will check has covered
    both the game's overall functionality and display features, while the OUnit
    testing tests the individual methods. As a result, we have demonstrated
    correctness of the code.*)

let command_parse_test (name : string) (command : string)
    (expected_output : command) =
  name >:: fun _ -> assert_equal expected_output (parse command)

let command_dir_parse_test (name : string) (command : string)
    (expected_output : command) =
  name >:: fun _ -> assert_equal expected_output (parse command)

let easy = Board.csv_array (Csv.load "easy.csv")
let medium = Board.csv_array (Csv.load "medium.csv")
let hard = Board.csv_array (Csv.load "hard.csv")

let is_border_test (name : string) (input1 : Board.t) (input2 : Board.position)
    (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (Board.is_border input1 input2)

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
    is_border_test "is_border (3,3) easy is true" easy (3, 3) false;
    is_border_test "is_border (3,3) medium is true" medium (3, 3) false;
    is_border_test "is_border (3,3) hard is true" hard (3, 3) false;
    is_border_test "is_border (14,14) easy is true" easy (14, 14) true;
    is_border_test "is_border (0,0) medium is true" medium (19, 19) true;
    is_border_test "is_border (0,0) hard is true" hard (21, 24) true;
  ]

let move_pac_test (name : string) (t : Logic.t) (board : Board.t)
    (dir : Command.dir) (expected_output : Logic.t) =
  name >:: fun _ -> assert_equal expected_output (move_pac t board dir)

let suite =
  "Test suite for Pac-Man" >::: List.flatten [ command_tests; logic_tests ]

let suite =
  "Test suite for Pac-Man"
  >::: List.flatten [ command_tests; logic_tests; board_tests ]

let _ = run_test_tt_main suite
