open OUnit2
open Chess
open Piece
open Board
open Controller
open Command
open View
open Printf

(** Test Plan: We decided to utilize glass-box testing, specifically trying to
    get make bisect to 90 or above percent. The modules that we decided to test
    were those that were exposed in the mli, as the methods that were not
    exposed were helper functions (so they would be covered by testing main
    functions). Since most of view and controller deal with a user input and the
    GUI, it was difficult to OUnit test them. In turn, we decided that it was
    best to OUnit test board, piece, and command, while view and controller were
    manually tested. Furthermore, we also tested board manaully as easier to
    find the edge cases while playing the game than through testing. The testing
    approach demonstrates correctness as it follows chess rules while playing
    the game. It also demonstrates correctness as our test cases for the
    features we fully implemented passed and the test cases for the features we
    did not fully implement were the only ones that didn't pass *)

(* loading file from data directory and turning into board *)
let data_dir_prefix = "data" ^ Filename.dir_sep
let default_json = Yojson.Basic.from_file (data_dir_prefix ^ "default.json")
let default = from_json default_json
let def_board = get_board default
let castle_json = Yojson.Basic.from_file (data_dir_prefix ^ "castle.json")
let castle = from_json castle_json
let castle_board = get_board castle
let empty_json = Yojson.Basic.from_file (data_dir_prefix ^ "empty.json")
let horde_json = Yojson.Basic.from_file (data_dir_prefix ^ "horde.json")
let horde = from_json horde_json
let horde_board = get_board horde

let horde_king_json =
  Yojson.Basic.from_file (data_dir_prefix ^ "horde_king.json")

let horde_king = from_json horde_king_json
let horde_king_board = get_board horde_king

(* [test_from_json_exn str json output] tests if there is an exception when
   reading in the json [json]*)
let test_from_json_exn str json output =
  str >:: fun _ ->
  assert_equal
    (try
       from_json json;
       false
     with _ -> true)
    output ~printer:string_of_bool

let controller_tests = [ test_from_json_exn "Empty json" empty_json true ]

(* helper functions to test command *)
let test_parse str input output =
  str >:: fun _ -> assert_equal (Command.parse input) output

(*[test_parse_exn str input output] tests if there is an exception when parsing
  a given input [input]*)
let test_parse_exn str input (output : bool) =
  str >:: fun _ ->
  assert_equal output
    (try
       Command.parse input;
       false
     with _ -> true)

(* helper functions to test piece *)
(*[test_string_of_piece_type str piece_type output] tests if a string if the
  method in Piece [string_of_piece_type] returns the piece type [output]*)
let test_string_of_piece_type str piece_type output =
  str >:: fun _ ->
  assert_equal
    (Piece.string_of_piece_type piece_type)
    output ~printer:String.escaped

(*[test_string_of_color str color output] tests if a string if the method in
  Piece [string_of_color] returns the piece type [output]*)
let test_string_of_color str color output =
  str >:: fun _ ->
  assert_equal (Piece.string_of_color color) output ~printer:String.escaped

(*[test_create str piece_type color x y output] tests if a piece created from
  string [str] will be a piece with a given piece type [piece_type] color
  [color] and coordinates [x],[y]*)
let test_create str piece_type color x y output =
  str >:: fun _ -> assert_equal (Piece.create piece_type color x y) output

(*[test_set_piece str piece_type color output] tests if the function set_piece
  in piece creates a piece with [piece_type] and color [color] that is
  equavilent to the expected [output]*)
let test_set_piece str piece_type color output =
  str >:: fun _ -> assert_equal (Piece.set_piece piece_type color) output

(*[test_piece_loc str piece output] tests if the location of [piece] is equal to
  the expected output, [output]*)
let test_piece_loc str piece output =
  str >:: fun _ -> assert_equal (Piece.piece_loc piece) output

(*[test_get_piece_type str piece output] tests if the get fucntion will
  succsessfully return the expected piece_type [output] given [piece]*)
let test_get_piece_type str piece output =
  str >:: fun _ -> assert_equal (Piece.get_piece_type piece) output

(*[test_get_piece_color str piece output] tests if the get fucntion will
  succsessfully return the expected color [output] given [piece]*)
let test_get_piece_color str piece output =
  str >:: fun _ -> assert_equal (Piece.get_piece_color piece) output

(* [test_print_board str t a b output] tests wether the board outputed with the
   given parameters [t a b] is equal to the expected output [output]*)
let test_print_board str t a b output =
  str >:: fun _ ->
  assert_equal (View.print_board t a b) output ~printer:String.escaped

(* board manipulation to test moves *)

let def_board1 = def_board
let def_board2 = def_board
let def_board3 = def_board
let def_board4 = def_board
let def_board5 = def_board
let def_board6 = def_board
let def_board7 = def_board
let def_board8 = def_board
let def_board9 = def_board
let pawn_wh_1 = create Pawn White 0 2
let pawn_wh_2 = create Pawn White 0 3
let pawn_bl_1 = create Pawn Black 0 5
let pawn_bl_2 = create Pawn Black 1 4
let pawn_cap = create Pawn Black 1 3
let knight_skip = create Knight White 2 2

let new_board_rook =
  pawn_wh_2 :: List.filter (fun x -> piece_loc x <> (0, 1)) def_board

let rook_vert = create Rook White 0 2

let new_board_rook_hor =
  rook_vert :: List.filter (fun x -> piece_loc x <> (0, 0)) new_board_rook

let new_board_rook_hor1 = new_board_rook_hor
let new_board_rook_hor2 = new_board_rook_hor
let rook_hor = create Rook White 7 2
let rook_down = create Rook White 0 0

let new_board_rook_cap =
  rook_hor :: List.filter (fun x -> piece_loc x <> (0, 2)) new_board_rook_hor

let rook_cap = create Rook White 7 6

let new_board_knight_neg =
  pawn_wh_2 :: knight_skip
  :: List.filter
       (fun x -> piece_loc x <> (0, 1) || piece_loc x <> (1, 0))
       new_board_rook

let knight_skip_neg = create Knight White 0 1

let king_move =
  create Pawn White 4 2
  :: List.filter (fun x -> piece_loc x <> (4, 1)) def_board

let king_move1 = king_move
let king_move2 = king_move
let king_wh_1 = create King White 4 1

let king_move_diag =
  king_wh_1 :: List.filter (fun x -> piece_loc x <> (4, 1)) king_move

let king_wh_diag = create King White 3 2

let king_move_lef =
  king_wh_diag :: List.filter (fun x -> piece_loc x <> (3, 2)) king_move_diag

let king_wh_lef = create King White 2 2
let bishop_wh = create Bishop White 1 4

let bishop_move =
  bishop_wh :: List.filter (fun x -> piece_loc x <> (5, 0)) king_move

let bishop_move1 = bishop_move
let bishop_move2 = bishop_move
let bishop_move3 = bishop_move
let bishop_neg = create Bishop White 0 3
let bishop_pos = create Bishop White 3 6
let bishop_back = create Bishop White 3 2
let queen_up = create Queen White 3 1

let queen_manip =
  queen_up :: create Pawn White 3 2
  :: List.filter
       (fun x -> piece_loc x <> (3, 0) && piece_loc x <> (3, 1))
       def_board

let queen_manip1 = queen_manip
let queen_manip2 = queen_manip
let queen_manip3 = queen_manip
let queen_diag_pos = create Queen White 6 4

(* normal board but only few pieces in certain places to check checkmate and
   related *)

let king_edge = create King White 0 0
let bishop_edge = create Bishop White 0 1
let queen_check = create Queen Black 1 2
let rook_support = create Rook Black 1 4
let king_win = create King Black 7 7

let check_board =
  [ king_edge; bishop_edge; queen_check; rook_support; king_win ]

let check_board1 = check_board
let check_board2 = check_board
let check_board3 = check_board
let check_board4 = check_board

let check_board_update =
  [ king_edge; create Queen Black 3 0; create Rook Black 1 0; king_win ]

let check_board_update1 = check_board_update
let check_board_update3 = check_board_update
let check_board_update2 = check_board_update
let check_board_update4 = check_board_update
let check_board_update5 = check_board_update
let check_board_update6 = check_board_update
let check_top_right = create King Black 7 7
let pawn_block = create Pawn Black 7 6
let knight_check = create Knight White 4 6
let bishop_check = create Bishop White 6 4

let bishop_mate_board =
  [
    check_top_right;
    pawn_block;
    knight_check;
    bishop_check;
    create King White 0 0;
  ]

let only_king_board = [ create King Black 0 2; create King White 0 0 ]

let draw_before =
  Board.update
    (Board.update
       (Board.update
          (Board.update (Board.update castle_board (4, 0) (4, 1)) (4, 7) (4, 6))
          (4, 1) (4, 0))
       (4, 6) (4, 7))
    (4, 0) (4, 1)

let draw = Board.update draw_before (4, 7) (4, 6)
let castle_board1 = castle_board
let castle_board2 = castle_board
let castle_board3 = castle_board
let castle_board4 = castle_board
let castle_board5 = castle_board

let castle_board_non_json =
  [
    create King Black 4 7;
    create King White 4 1;
    create Rook White 0 0;
    create Rook White 7 0;
  ]

let castle_board_non_json1 = castle_board_non_json
let castle_board_non_json2 = castle_board_non_json
let castle_board_non_json3 = castle_board_non_json
let castle_board_non_json4 = castle_board_non_json
let castle_board_non_json5 = castle_board_non_json
let bishop_mate_board1 = bishop_mate_board
let only_king_board1 = only_king_board

let top_left_check =
  [ create King Black 0 7; create King White 7 0; create Rook White 1 0 ]

let bot_right_check =
  [ create King Black 0 7; create King White 7 0; create Rook Black 6 7 ]

(* helper functions to test board *)
let piece_to_string p =
  match Piece.piece_loc p with
  | x, y -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let rec list_to_string f x =
  match x with
  | [] -> ""
  | h :: t -> f h ^ "\n" ^ list_to_string f t

let test_update str t init fin output =
  str >:: fun _ ->
  Board.clear_moves;
  assert_equal output (Board.update t init fin)
    ~printer:(list_to_string piece_to_string)

let test_update_exn str t init fin (output : bool) =
  str >:: fun _ ->
  Board.clear_moves;
  assert_equal output
    (try
       Board.update t init fin;
       false
     with _ -> true)
    ~printer:string_of_bool

(* test suites *)
let command_tests =
  [
    test_parse "command help" "help" Help;
    test_parse "command quit" "quit" Quit;
    test_parse "command info" "info" Info;
    test_parse "no command" "" Empty;
    test_parse "command load" "load" (Load "placeholder");
    test_parse "command help" "castle" (Castle "");
    test_parse "command move" "move a1 a2" (Move ((0, 0), (0, 1)));
    test_parse_exn "command unknown" "unknown" true;
  ]

let piece_tests =
  [
    test_string_of_color "Color White" White "white";
    test_string_of_color "Color black" Black "black";
    test_string_of_piece_type "Piece king" King "king";
    test_string_of_piece_type "Piece pawn" Pawn "pawn";
    test_set_piece "White king at left corner" King White king_edge;
    test_create "White rook at left corner" Rook White 0 0
      (create Rook White 0 0);
    test_get_piece_color "Black king" king_win Black;
    test_get_piece_color "White pawn" pawn_wh_1 White;
    test_get_piece_type "Black queen" queen_check Queen;
    test_get_piece_type "White bishop" bishop_edge Bishop;
    test_piece_loc "left corner" king_edge (0, 0);
  ]

let view_tests =
  [
    test_print_board "print default board" def_board "default" "white"
      "\n\
       Playing board \"default\", white to move\n\n\
      \  8 \226\153\150 \226\153\152 \226\153\151 \226\153\149 \226\153\148 \
       \226\153\151 \226\153\152 \226\153\150 \n\
      \  7 \226\153\153 \226\153\153 \226\153\153 \226\153\153 \226\153\153 \
       \226\153\153 \226\153\153 \226\153\153 \n\
      \  6 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  5 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  4 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  3 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  2 \226\153\159\239\184\142 \226\153\159\239\184\142 \
       \226\153\159\239\184\142 \226\153\159\239\184\142 \
       \226\153\159\239\184\142 \226\153\159\239\184\142 \
       \226\153\159\239\184\142 \226\153\159\239\184\142 \n\
      \  1 \226\153\156 \226\153\158 \226\153\157 \226\153\155 \226\153\154 \
       \226\153\157 \226\153\158 \226\153\156 \n\
      \    a b c d e f g h\n";
    test_print_board "print castle board" castle_board "castle" "white"
      "\n\
       Playing board \"castle\", white to move\n\n\
      \  8 \226\153\150 \194\183 \194\183 \194\183 \226\153\148 \194\183 \
       \194\183 \226\153\150 \n\
      \  7 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  6 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  5 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  4 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  3 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  2 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  1 \226\153\156 \194\183 \194\183 \194\183 \226\153\154 \194\183 \
       \194\183 \226\153\156 \n\
      \    a b c d e f g h\n";
    test_print_board "print check board" check_board "checkmate" "white"
      "\n\
       Playing board \"checkmate\", white to move\n\n\
      \  8 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \226\153\148 \n\
      \  7 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  6 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  5 \194\183 \226\153\150 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  4 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  3 \194\183 \226\153\149 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  2 \226\153\157 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \  1 \226\153\154 \194\183 \194\183 \194\183 \194\183 \194\183 \194\183 \
       \194\183 \n\
      \    a b c d e f g h\n";
  ]

let board_tests =
  [
    test_update "white pawn moving by 1" def_board (0, 1) (0, 2)
      (pawn_wh_1 :: List.filter (fun x -> piece_loc x <> (0, 1)) def_board);
    test_update "white pawn moving by 2" def_board1 (0, 1) (0, 3)
      (pawn_wh_2 :: List.filter (fun x -> piece_loc x <> (0, 1)) def_board1);
    test_update_exn "white pawn cannot move 2"
      (Board.update def_board2 (0, 1) (0, 2))
      (0, 2) (0, 4) true;
    test_update_exn "black pawn blocked"
      (Board.update (Board.update def_board3 (0, 1) (0, 3)) (0, 3) (0, 4))
      (0, 6) (0, 4) true;
    test_update "black pawn moving by 1" def_board4 (0, 6) (0, 5)
      (pawn_bl_1 :: List.filter (fun x -> piece_loc x <> (0, 6)) def_board4);
    test_update "black pawn moving by 2" def_board5 (1, 6) (1, 4)
      (pawn_bl_2 :: List.filter (fun x -> piece_loc x <> (1, 6)) def_board5);
    test_update "black pawn capture"
      (Board.update (Board.update def_board6 (0, 6) (0, 4)) (1, 1) (1, 3))
      (0, 4) (1, 3)
      (pawn_cap
      :: List.filter
           (fun x -> piece_loc x <> (0, 6) && piece_loc x <> (1, 1))
           def_board9);
    test_update "white knight skip" def_board7 (1, 0) (2, 2)
      (knight_skip :: List.filter (fun x -> piece_loc x <> (1, 0)) def_board7);
    test_update "white rook vertical up" new_board_rook (0, 0) (0, 2)
      (rook_vert :: List.filter (fun x -> piece_loc x <> (0, 0)) new_board_rook);
    test_update "white rook vertical down" new_board_rook_hor (0, 2) (0, 0)
      (rook_down
      :: List.filter (fun x -> piece_loc x <> (0, 2)) new_board_rook_hor);
    test_update "white rook horizontal" new_board_rook_hor1 (0, 2) (7, 2)
      (rook_hor
      :: List.filter (fun x -> piece_loc x <> (0, 2)) new_board_rook_hor1);
    test_update_exn "white rook hor error"
      (Board.update new_board_rook_hor2 (2, 1) (2, 2))
      (0, 2) (7, 2) true;
    test_update "white rook cap" new_board_rook_cap (7, 2) (7, 6)
      (rook_cap
      :: List.filter
           (fun x -> piece_loc x <> (7, 2) && piece_loc x <> (7, 6))
           new_board_rook_cap);
    test_update "white knight negative" new_board_knight_neg (2, 2) (0, 1)
      (knight_skip_neg
      :: List.filter (fun x -> piece_loc x <> (2, 2)) new_board_knight_neg);
    test_update "white king up 1" king_move (4, 0) (4, 1)
      (king_wh_1 :: List.filter (fun x -> piece_loc x <> (4, 0)) king_move);
    test_update "white king down 1"
      (Board.update king_move1 (4, 0) (4, 1))
      (4, 1) (4, 0)
      (create King White 4 0
      :: List.filter (fun x -> piece_loc x <> (4, 0)) king_move1);
    test_update "white king diag 1" king_move_diag (4, 1) (3, 2)
      (king_wh_diag
      :: List.filter (fun x -> piece_loc x <> (4, 1)) king_move_diag);
    test_update "white king left 1" king_move_lef (3, 2) (2, 2)
      (king_wh_lef :: List.filter (fun x -> piece_loc x <> (3, 2)) king_move_lef);
    test_update "white bishop diag left up" king_move2 (5, 0) (1, 4)
      (bishop_wh :: List.filter (fun x -> piece_loc x <> (5, 0)) king_move2);
    test_update "white bishop diag left down" bishop_move (1, 4) (0, 3)
      (bishop_neg :: List.filter (fun x -> piece_loc x <> (1, 4)) bishop_move);
    test_update "white bishop diag right up" bishop_move2 (1, 4) (3, 6)
      (bishop_pos
      :: List.filter
           (fun x -> piece_loc x <> (1, 4) && piece_loc x <> (3, 6))
           bishop_move2);
    test_update "white bishop diag right down" bishop_move1 (1, 4) (3, 2)
      (bishop_back :: List.filter (fun x -> piece_loc x <> (1, 4)) bishop_move1);
    test_update "white queen up"
      (Board.update def_board (3, 1) (3, 2))
      (3, 0) (3, 1) queen_manip;
    test_update "white queen hor"
      (Board.update queen_manip (3, 1) (6, 4))
      (6, 4) (4, 4)
      (create Queen White 4 4
      :: List.filter (fun x -> piece_loc x <> (3, 1)) queen_manip);
    test_update "white queen diag up right" queen_manip1 (3, 1) (6, 4)
      (queen_diag_pos
      :: List.filter (fun x -> piece_loc x <> (3, 1)) queen_manip1);
    test_update "white queen diag left down"
      (Board.update queen_manip2 (3, 1) (6, 4))
      (6, 4) (3, 1) queen_manip2;
    test_update "white queen diag right down"
      (Board.update queen_manip3 (3, 1) (1, 3))
      (1, 3) (3, 1) queen_manip3;
    test_update_exn "white rook over white pawn" def_board8 (0, 0) (0, 2) true;
    test_update_exn "white bishop not moving diagonally" bishop_move3 (1, 4)
      (3, 4) true;
    test_update "black queen causing check" check_board (1, 2) (0, 1)
      (create Queen Black 0 1
      :: List.filter
           (fun x -> piece_loc x <> (1, 2) && piece_loc x <> (0, 1))
           check_board);
    test_update "white king escaping check"
      (Board.update check_board1 (1, 2) (0, 1))
      (0, 0) (0, 1)
      (create King White 0 1
      :: List.filter
           (fun x ->
             piece_loc x <> (1, 2)
             && piece_loc x <> (0, 1)
             && piece_loc x <> (0, 0))
           check_board1);
    test_update "black queen causing check with bishop blocking"
      (Board.update check_board2 (1, 2) (3, 0))
      (0, 1) (1, 0)
      (create Bishop White 1 0 :: create Queen Black 3 0
      :: List.filter
           (fun x -> piece_loc x <> (1, 2) && piece_loc x <> (0, 1))
           check_board2);
    test_update "escaping check by moving away" check_board_update5 (0, 0) (0, 1)
      (create King White 0 1
      :: List.filter (fun x -> piece_loc x <> (0, 0)) check_board_update5);
    test_update "white bishop capturing queen"
      (Board.update check_board3 (1, 2) (1, 0))
      (0, 1) (1, 0)
      (create Bishop White 1 0
      :: List.filter
           (fun x -> piece_loc x <> (1, 2) && piece_loc x <> (0, 1))
           check_board3);
    test_update_exn "queen causing checkmate"
      (Board.update check_board4 (0, 1) (1, 0))
      (1, 2) (1, 0) true;
    test_update_exn "bishop causing checkmate" bishop_mate_board (6, 4) (5, 5)
      true;
    test_update_exn "king not valid check move"
      (Board.update
         (List.filter (fun x -> piece_loc x <> (4, 6)) bishop_mate_board1)
         (6, 4) (5, 5))
      (7, 7) (6, 6) true;
    test_update_exn "king camnot move next to opposing king" only_king_board
      (0, 2) (0, 1) true;
    test_update_exn "there is only one king"
      (List.filter (fun x -> piece_loc x <> (0, 0)) only_king_board1)
      (0, 2) (0, 1) true;
    test_update "castle on white king side" castle_board_non_json1 (4, 7) (7, 7)
      (create Rook White 5 0 :: create King White 6 0
      :: List.filter
           (fun x -> piece_loc x <> (7, 7) && piece_loc x <> (4, 7))
           castle_board_non_json1);
    test_update "castle on white queen side with rook first"
      castle_board_non_json2 (0, 7) (4, 7)
      (create Rook White 2 0 :: create King White 1 0
      :: List.filter
           (fun x -> piece_loc x <> (0, 7) && piece_loc x <> (4, 7))
           castle_board_non_json2);
    test_update "castle on white queen side" castle_board_non_json3 (4, 7) (0, 7)
      (create Rook White 2 0 :: create King White 1 0
      :: List.filter
           (fun x -> piece_loc x <> (0, 7) && piece_loc x <> (4, 7))
           castle_board_non_json3);
    test_update "castle on black king side" castle_board1 (4, 7) (7, 7)
      (create Rook Black 5 7 :: create King Black 6 7
      :: List.filter
           (fun x -> piece_loc x <> (7, 7) && piece_loc x <> (4, 7))
           castle_board1);
    test_update "castle on black king side with init rook" castle_board1 (7, 7)
      (4, 7)
      (create Rook Black 5 7 :: create King Black 6 7
      :: List.filter
           (fun x -> piece_loc x <> (7, 7) && piece_loc x <> (4, 7))
           castle_board1);
    test_update "castle on black queen side" castle_board2 (4, 7) (0, 7)
      (create Rook Black 2 7 :: create King Black 1 7
      :: List.filter
           (fun x -> piece_loc x <> (0, 7) && piece_loc x <> (4, 7))
           castle_board2);
    test_update_exn "cannot castle, white king in wrong position"
      (Board.update castle_board3 (4, 0) (4, 1))
      (4, 1) (7, 0) true;
    test_update_exn "cannot castle, white king moved to same spot"
      (Board.update castle_board_non_json4 (4, 1) (4, 0))
      (0, 0) (4, 0) true;
    test_update_exn "cannot castle, piece on path"
      (create Knight White 1 0 :: castle_board4)
      (4, 0) (0, 0) true;
    test_update_exn "draw by repition" draw (4, 1) (4, 0) true;
  ]

let unique_board_tests =
  [
    test_update_exn "multiple kings on board" horde_king_board (0, 7) (0, 6)
      true;
    test_update_exn "cannot move pawn 2 due to starting past default"
      horde_board (7, 3) (7, 5) true;
    test_update_exn "cannot castle when not rook at final"
      (create Pawn White 0 0
      :: List.filter (fun x -> piece_loc x <> (0, 0)) castle_board)
      (4, 0) (0, 0) true;
    test_update_exn "cannot castle when not king at final"
      (create Pawn White 4 0 :: create King White 5 0
      :: List.filter (fun x -> piece_loc x <> (4, 0)) castle_board)
      (0, 0) (4, 0) true;
    test_update_exn "cannot castle, white rook moved to same spot"
      (Board.update (Board.update castle_board (0, 0) (0, 1)) (0, 1) (0, 0))
      (0, 0) (4, 0) true;
    test_update "black knight stopping check by blocking path"
      (Board.update
         (create Knight Black 4 5 :: bishop_mate_board)
         (6, 4) (5, 5))
      (4, 5) (6, 6)
      (create Knight Black 6 6 :: create Bishop White 5 5
      :: List.filter (fun x -> piece_loc x <> (6, 4)) bishop_mate_board);
    test_update "black king check in top left" top_left_check (1, 0) (0, 0)
      (create Rook White 0 0
      :: List.filter (fun x -> piece_loc x <> (1, 0)) top_left_check);
    test_update "white king check in bot right" bot_right_check (6, 7) (7, 7)
      (create Rook Black 7 7
      :: List.filter (fun x -> piece_loc x <> (6, 7)) bot_right_check);
    test_update_exn "invalid white pawn move" def_board (0, 1) (0, 1) true;
    test_update_exn "invalid white knight move" def_board (1, 0) (0, 3) true;
    test_update_exn "cannot move own other piece when in check"
      (Board.update bishop_mate_board (6, 4) (5, 5))
      (7, 6) (7, 5) true;
    test_update_exn "moving onto piece of same color" def_board (0, 0) (0, 1)
      true;
    test_update_exn "king not allowed to move into check" bishop_mate_board
      (7, 7) (6, 7) true;
    test_update_exn "cannot castle because will be in check"
      (create Queen White 1 1 :: castle_board)
      (4, 7) (0, 7) true;
    test_update "black pawn causing check"
      [ create Pawn Black 1 2; create King Black 7 7; create King White 0 0 ]
      (1, 2) (1, 1)
      [ create Pawn Black 1 1; create King Black 7 7; create King White 0 0 ];
    test_update "black pawn causing check by moving two"
      [ create Pawn Black 1 6; create King Black 2 5; create King White 0 3 ]
      (1, 6) (1, 4)
      [ create Pawn Black 1 4; create King Black 2 5; create King White 0 3 ];
    test_update_exn "cannot capture pawn that is checking"
      [ create Pawn Black 1 1; create King Black 2 2; create King White 0 0 ]
      (0, 0) (1, 1) true;
    test_update "black pawn moving two to block path"
      (create Pawn Black 4 6
      :: [
           create King Black 7 7; create King White 0 0; create Bishop White 3 3;
         ])
      (4, 6) (4, 4)
      (create Pawn Black 4 4
      :: [
           create King Black 7 7; create King White 0 0; create Bishop White 3 3;
         ]);
    test_update_exn "invalid move to block path; open new path"
      (create Rook White 0 7 :: create Pawn Black 6 7 :: create Bishop White 5 5
      :: List.filter (fun x -> piece_loc x <> (6, 4)) bishop_mate_board)
      (6, 7) (6, 6) true;
    test_update_exn "stalemate/draw when only moves will result in check"
      (Board.update
         (create Knight Black 0 1
         :: [
              create King White 0 0;
              create Pawn Black 0 2;
              create Rook Black 6 7;
              create King Black 7 7;
            ])
         (6, 7) (1, 7))
      (0, 0) (1, 0) true;
    test_update_exn "draw when there are only king of each on the board"
      (Board.update
         (create Knight Black 4 5
         :: [ create King White 5 5; create King Black 7 7 ])
         (5, 5) (4, 5))
      (7, 7) (6, 6) true;
    test_update "move bishop right down more than 2"
      (Board.update
         (Board.update bishop_mate_board (4, 6) (2, 5))
         (6, 4) (3, 7))
      (3, 7) (6, 4)
      (create Bishop White 6 4 :: create Knight White 2 5
      :: List.filter
           (fun x -> piece_loc x <> (6, 4) && piece_loc x <> (4, 6))
           bishop_mate_board);
    test_update "synthesis 1"
      (Board.update
         (Board.update bishop_mate_board (4, 6) (2, 5))
         (6, 4) (3, 7))
      (3, 7) (6, 4)
      (create Bishop White 6 4 :: create Knight White 2 5
      :: List.filter
           (fun x -> piece_loc x <> (6, 4) && piece_loc x <> (4, 6))
           bishop_mate_board);
    test_update_exn "in stalemate because all pieces cannot move"
      [
        create King White 0 0;
        create Pawn White 0 1;
        create Pawn Black 0 2;
        create Pawn White 7 1;
        create Pawn Black 7 2;
        create King Black 2 0;
      ]
      (0, 0) (1, 0) true;
    test_update_exn "castling two pieces that are not a king and rook"
      [
        create King White 7 0;
        create King Black 7 7;
        create Knight White 4 0;
        create Bishop White 0 0;
      ]
      (0, 0) (4, 0) true;
    test_update_exn "king move causing stalemate"
      [ create King White 0 0; create Rook Black 7 1; create King Black 3 0 ]
      (3, 0) (2, 0) true;
    test_update_exn "white pawn moving two but below 2 "
      [ create King White 0 0; create King Black 7 7; create Pawn White 1 0 ]
      (1, 0) (1, 2) true;
    test_update "white pawn moving 1 and then two"
      (Board.update
         [ create King White 0 0; create King Black 7 7; create Pawn White 1 0 ]
         (1, 0) (1, 1))
      (1, 1) (1, 3)
      (create Pawn White 1 3
      :: List.filter
           (fun x -> piece_loc x <> (1, 0))
           [
             create King White 0 0; create King Black 7 7; create Pawn White 1 0;
           ]);
  ]

let suite =
  "test suite for Irish Chess"
  >::: List.flatten
         [
           controller_tests;
           command_tests;
           piece_tests;
           view_tests;
           board_tests;
           unique_board_tests;
         ]

let _ = run_test_tt_main suite