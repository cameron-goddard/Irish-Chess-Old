open OUnit2
open Chess
open Piece
open Board
open Controller
open Command
open Printf

(* loading file from data directory and turning into board *)
let data_dir_prefix = "data" ^ Filename.dir_sep
let default_json = Yojson.Basic.from_file (data_dir_prefix ^ "default.json")
let default = from_json default_json
let def_board = get_board default

(* helper functions to test command *)
let test_parse str input output =
  str >:: fun _ -> assert_equal (Command.parse input) output

let test_parse_exn str input (output : bool) =
  str >:: fun _ ->
  assert_equal output
    (try
       Command.parse input;
       false
     with _ -> true)

(* helper functions to test piece *)

let test_string_of_piece_type str piece_type output =
  str >:: fun _ ->
  assert_equal
    (Piece.string_of_piece_type piece_type)
    output ~printer:String.escaped

let test_string_of_color str color output =
  str >:: fun _ ->
  assert_equal (Piece.string_of_color color) output ~printer:String.escaped

let test_create str piece_type color x y output =
  str >:: fun _ -> assert_equal (Piece.create piece_type color x y) output

let test_set_piece str piece_type color output =
  str >:: fun _ -> assert_equal (Piece.set_piece piece_type color) output

let test_piece_loc str piece output =
  str >:: fun _ -> assert_equal (Piece.piece_loc piece) output

let test_get_piece_type str piece output =
  str >:: fun _ -> assert_equal (Piece.get_piece_type piece) output

let test_get_piece_color str piece output =
  str >:: fun _ -> assert_equal (Piece.get_piece_color piece) output

(* board manipulation to test moves *)

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

let bishop_neg = create Bishop White 0 3
let bishop_pos = create Bishop White 3 6
let bishop_back = create Bishop White 3 2
let queen_up = create Queen White 3 1

let queen_manip =
  queen_up :: create Pawn White 3 2
  :: List.filter
       (fun x -> piece_loc x <> (3, 0) && piece_loc x <> (3, 1))
       def_board

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

let check_board_update =
  [ king_edge; create Queen Black 3 0; create Rook Black 1 0; king_win ]

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
  assert_equal (Board.update t init fin) output
    ~printer:(list_to_string piece_to_string)

let test_update_exn str t init fin (output : bool) =
  str >:: fun _ ->
  assert_equal output
    (try
       Board.update t init fin;
       false
     with _ -> true)

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

let board_tests =
  [
    test_update "white pawn moving by 1" def_board (0, 1) (0, 2)
      (pawn_wh_1 :: List.filter (fun x -> piece_loc x <> (0, 1)) def_board);
    test_update "white pawn moving by 2" def_board (0, 1) (0, 3)
      (pawn_wh_2 :: List.filter (fun x -> piece_loc x <> (0, 1)) def_board);
    test_update_exn "white pawn cannot move 2"
      (Board.update def_board (0, 1) (0, 2))
      (0, 2) (0, 4) true;
    test_update_exn "black pawn blocked"
      (Board.update (Board.update def_board (0, 1) (0, 3)) (0, 3) (0, 4))
      (0, 6) (0, 4) true;
    test_update "black pawn moving by 1" def_board (0, 6) (0, 5)
      (pawn_bl_1 :: List.filter (fun x -> piece_loc x <> (0, 6)) def_board);
    test_update "black pawn moving by 2" def_board (1, 6) (1, 4)
      (pawn_bl_2 :: List.filter (fun x -> piece_loc x <> (1, 6)) def_board);
    test_update "black pawn capture"
      (Board.update (Board.update def_board (0, 6) (0, 4)) (1, 1) (1, 3))
      (0, 4) (1, 3)
      (pawn_cap
      :: List.filter
           (fun x -> piece_loc x <> (0, 6) && piece_loc x <> (1, 1))
           def_board);
    test_update "white knight skip" def_board (1, 0) (2, 2)
      (knight_skip :: List.filter (fun x -> piece_loc x <> (1, 0)) def_board);
    test_update "white rook vertical up" new_board_rook (0, 0) (0, 2)
      (rook_vert :: List.filter (fun x -> piece_loc x <> (0, 0)) new_board_rook);
    test_update "white rook vertical down" new_board_rook_hor (0, 2) (0, 0)
      (rook_down
      :: List.filter (fun x -> piece_loc x <> (0, 2)) new_board_rook_hor);
    test_update "white rook horizontal" new_board_rook_hor (0, 2) (7, 2)
      (rook_hor
      :: List.filter (fun x -> piece_loc x <> (0, 2)) new_board_rook_hor);
    test_update_exn "white rook hor error"
      (Board.update new_board_rook_hor (2, 1) (2, 2))
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
      (Board.update king_move (4, 0) (4, 1))
      (4, 1) (4, 0)
      (create King White 4 0
      :: List.filter (fun x -> piece_loc x <> (4, 0)) king_move);
    test_update "white king diag 1" king_move_diag (4, 1) (3, 2)
      (king_wh_diag
      :: List.filter (fun x -> piece_loc x <> (4, 1)) king_move_diag);
    test_update "white king left 1" king_move_lef (3, 2) (2, 2)
      (king_wh_lef :: List.filter (fun x -> piece_loc x <> (3, 2)) king_move_lef);
    test_update "white bishop diag left up" king_move (5, 0) (1, 4)
      (bishop_wh :: List.filter (fun x -> piece_loc x <> (5, 0)) king_move);
    test_update "white bishop diag left down" bishop_move (1, 4) (0, 3)
      (bishop_neg :: List.filter (fun x -> piece_loc x <> (1, 4)) bishop_move);
    test_update "white bishop diag right up" bishop_move (1, 4) (3, 6)
      (bishop_pos
      :: List.filter
           (fun x -> piece_loc x <> (1, 4) && piece_loc x <> (3, 6))
           bishop_move);
    test_update "white bishop diag right down" bishop_move (1, 4) (3, 2)
      (bishop_back :: List.filter (fun x -> piece_loc x <> (1, 4)) bishop_move);
    test_update "white queen up"
      (Board.update def_board (3, 1) (3, 2))
      (3, 0) (3, 1) queen_manip;
    test_update "white queen diag up right" queen_manip (3, 1) (6, 4)
      (queen_diag_pos
      :: List.filter (fun x -> piece_loc x <> (3, 1)) queen_manip);
    test_update "white queen diag left down"
      (Board.update queen_manip (3, 1) (6, 4))
      (6, 4) (3, 1) queen_manip;
    test_update "white queen diag right down"
      (Board.update queen_manip (3, 1) (1, 3))
      (1, 3) (3, 1) queen_manip;
    test_update_exn "white rook over white pawn" def_board (0, 0) (0, 2) true;
    test_update_exn "white bishop not moving diagonally" bishop_move (1, 4)
      (3, 4) true;
    test_update "black queen causing check" check_board (1, 2) (0, 1)
      (create Queen Black 0 1
      :: List.filter
           (fun x -> piece_loc x <> (1, 2) && piece_loc x <> (0, 1))
           check_board);
    test_update "white kign escaping check"
      (Board.update check_board (1, 2) (0, 1))
      (0, 0) (0, 1)
      (create King White 0 1
      :: List.filter
           (fun x ->
             piece_loc x <> (1, 2)
             && piece_loc x <> (0, 1)
             && piece_loc x <> (0, 0))
           check_board);
    test_update "black queen causing check with bishop blocking"
      (Board.update check_board (1, 2) (0, 3))
      (0, 1) (1, 0)
      (create Bishop White 1 0 :: create Queen Black 0 1
      :: List.filter
           (fun x -> piece_loc x <> (1, 2) && piece_loc x <> (0, 1))
           check_board);
    test_update "escaping check by moving away" check_board_update (0, 0) (0, 1)
      (create King White 0 1
      :: List.filter (fun x -> piece_loc x <> (0, 0)) check_board_update);
    test_update "white bishop capturing queen"
      (Board.update check_board (1, 3) (1, 0))
      (0, 1) (1, 0)
      (create Bishop White 1 0
      :: List.filter
           (fun x -> piece_loc x <> (1, 3) && piece_loc x <> (0, 1))
           check_board);
  ]

(* test_update "knight capturing" new_t (2, 4) (1, 2) new_t_cap; test_update
   "king cant move due to check prev" new_t_cap (1, 0) (2, 0) new_t; test_update
   "checkmate yessir" checkmate (4, 6) (0, 6) new_t; test_update "castle" castle
   (0, 7) (4, 7) [ create King Black 2 7; create King Black 1 7; king_whc_ca;
   queen ]; *)

let suite =
  "test suite for Irish Chess"
  >::: List.flatten [ command_tests; piece_tests; board_tests ]

let _ = run_test_tt_main suite