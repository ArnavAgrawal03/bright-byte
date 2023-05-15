open OUnit2
open Game
open Command
(* open Logic *)
(* open State open Board *)

(** *)

(* let dir_stiring d = match d with | Up -> "up" | Left -> "left" | Down ->
   "down" | Right -> "right" *)
(* let string_of_position position = match position with | x, y -> "(" ^
   string_of_int x ^ ", " ^ string_of_int y ^ ")" *)

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

let suite = "Test suite for Pac-Man" >::: List.flatten [ command_tests ]
let _ = run_test_tt_main suite
