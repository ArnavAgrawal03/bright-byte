open OUnit2
open Game
open Command
open Logic
(* open State open Board open Ghosts*)

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

let move_pac_test (name : string) (t : Logic.t) (board : Board.t)
    (dir : Command.dir) (expected_output : Logic.t) =
  name >:: fun _ -> assert_equal expected_output (move_pac t board dir)

let make_og_pos_test (name : string) (t : Logic.t) (expected_output : Logic.t) =
  name >:: fun _ -> assert_equal expected_output (make_og_pos t)

let suite = "Test suite for Pac-Man" >::: List.flatten [ command_tests ]
let _ = run_test_tt_main suite
