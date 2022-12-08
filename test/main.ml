open OUnit2
open Chess
open Piece
open Board
open Printf

let pawn_wh = create_piece Pawn White 1 1
let king_wh = create_piece King White 1 0
let pawn_bl = create_piece Pawn Black 1 4
let king_bl = create_piece King Black 1 5
let knight_bl = create_piece Knight Black 2 4
let pawn_bl2 = create_piece Pawn Black 3 4
let t = [ pawn_wh; pawn_bl; pawn_bl2; knight_bl; king_wh; king_bl ]
let pawn_wh2 = create_piece Pawn White 1 2
let knight_bl2 = create_piece Knight Black 1 2
let new_t = [ pawn_wh2; pawn_bl; pawn_bl2; knight_bl; king_wh; king_bl ]
let new_t_cap = [ knight_bl2; pawn_bl; pawn_bl2; king_wh; king_bl ]
let pawn_own = create_piece Pawn Black 0 0
let pawn_own2 = create_piece Pawn Black 0 1
let own = [ pawn_own; pawn_own2 ]
let king_whc = create_piece King White 0 0
let queen = create_piece Queen Black 4 6
let rook = create_piece Rook Black 1 5
let king_blc = create_piece King Black 0 7
let checkmate = [ king_whc; queen; rook; king_blc ]

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

let tests =
  "board test suite"
  >::: [
         test_update "pawn moving by 1" t (1, 1) (1, 2) new_t;
         test_update "knight capturing" new_t (2, 4) (1, 2) new_t_cap;
         (* test_update "pawn moving 2" new_t_cap (3, 4) (1, 2) new_t; *)
         test_update "king cant move due to check prev" new_t_cap (1, 0) (2, 0)
           new_t;
         test_update "cant move onto own team" own (0, 0) (0, 1) new_t;
         test_update "checkmate yessir" checkmate (4, 6) (0, 6) new_t;
       ]

let _ = run_test_tt_main tests