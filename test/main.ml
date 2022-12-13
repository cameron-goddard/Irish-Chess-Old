open OUnit2
open Chess
open Piece
open Board
open Controller
open Printf

(* loading file from data directory and turning into board *)
let data_dir_prefix = "data" ^ Filename.dir_sep
let default_json = Yojson.Basic.from_file (data_dir_prefix ^ "default.json")
let default = from_json default_json
let def_board = get_board default

(* board manipulation to test moves *)

let pawn_wh_1 = create Pawn White 0 2
let pawn_wh_2 = create Pawn White 0 3
let pawn_bl_1 = create Pawn Black 0 5
let pawn_bl_2 = create Pawn Black 1 4
let knight_skip = create Knight White 2 2

let new_board_rook =
  pawn_wh_2 :: List.filter (fun x -> piece_loc x <> (0, 1)) def_board

let rook_vert = create Rook White 0 2

let new_board_rook_hor =
  rook_vert :: List.filter (fun x -> piece_loc x <> (0, 0)) new_board_rook

let rook_hor = create Rook White 7 2

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

exception InvalidSomething

let test_update_exn str t init fin (output : exn) =
  str >:: fun _ -> assert_raises output (fun () -> Board.update t init fin)

let tests =
  "board test suite"
  >::: [
         test_update "white pawn moving by 1" def_board (0, 1) (0, 2)
           (pawn_wh_1 :: List.filter (fun x -> piece_loc x <> (0, 1)) def_board);
         test_update "white pawn moving by 2" def_board (0, 1) (0, 3)
           (pawn_wh_2 :: List.filter (fun x -> piece_loc x <> (0, 1)) def_board);
         test_update "black pawn moving by 1" def_board (0, 6) (0, 5)
           (pawn_bl_1 :: List.filter (fun x -> piece_loc x <> (0, 6)) def_board);
         test_update "black pawn moving by 2" def_board (1, 6) (1, 4)
           (pawn_bl_2 :: List.filter (fun x -> piece_loc x <> (1, 6)) def_board);
         test_update "white knight skip" def_board (1, 0) (2, 2)
           (knight_skip
           :: List.filter (fun x -> piece_loc x <> (1, 0)) def_board);
         test_update "white rook vertical" new_board_rook (0, 0) (0, 2)
           (rook_vert
           :: List.filter (fun x -> piece_loc x <> (0, 0)) new_board_rook);
         test_update "white rook horizontal" new_board_rook_hor (0, 2) (7, 2)
           (rook_hor
           :: List.filter (fun x -> piece_loc x <> (0, 2)) new_board_rook_hor);
         test_update "white rook cap" new_board_rook_cap (7, 2) (7, 6)
           (rook_cap
           :: List.filter
                (fun x -> piece_loc x <> (7, 2) && piece_loc x <> (7, 6))
                new_board_rook_cap);
         test_update "white knight negative" new_board_knight_neg (2, 2) (0, 1)
           (knight_skip_neg
           :: List.filter (fun x -> piece_loc x <> (2, 2)) new_board_knight_neg
           );
         test_update "white king up 1" king_move (4, 0) (4, 1)
           (king_wh_1 :: List.filter (fun x -> piece_loc x <> (4, 0)) king_move);
         test_update "white king diag 1" king_move_diag (4, 1) (3, 2)
           (king_wh_diag
           :: List.filter (fun x -> piece_loc x <> (4, 1)) king_move_diag);
         test_update "white king left 1" king_move_lef (3, 2) (2, 2)
           (king_wh_lef
           :: List.filter (fun x -> piece_loc x <> (3, 2)) king_move_lef);
         test_update "white bishop diag left up" king_move (5, 0) (1, 4)
           (bishop_wh :: List.filter (fun x -> piece_loc x <> (5, 0)) king_move);
         test_update "white bishop diag left down" bishop_move (1, 4) (0, 3)
           (bishop_neg
           :: List.filter (fun x -> piece_loc x <> (1, 4)) bishop_move);
         test_update "white bishop diag right up" bishop_move (1, 4) (3, 6)
           (bishop_pos
           :: List.filter
                (fun x -> piece_loc x <> (1, 4) && piece_loc x <> (3, 6))
                bishop_move);
         test_update "white bishop diag right down" bishop_move (1, 4) (3, 2)
           (bishop_back
           :: List.filter (fun x -> piece_loc x <> (1, 4)) bishop_move);
         test_update_exn "white rook to white pawn" def_board (0, 0) (0, 2)
           InvalidSomething;
       ]

(* test_update "knight capturing" new_t (2, 4) (1, 2) new_t_cap; test_update
   "king cant move due to check prev" new_t_cap (1, 0) (2, 0) new_t; test_update
   "cant move onto own team" own (0, 0) (0, 1) new_t; test_update "checkmate
   yessir" checkmate (4, 6) (0, 6) new_t; test_update "castle" castle (0, 7) (4,
   7) [ create King Black 2 7; create King Black 1 7; king_whc_ca; queen ]; *)

let _ = run_test_tt_main tests