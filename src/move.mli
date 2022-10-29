(* gives the format of the type of moves *)
(*type moves

(*type conditional_move*)
(*a list of moves*)
type move_list
(* given a type of piece returns the list of possible moves for the piece*)
val moves_for_piece: Piece.piece_type -> move_list
(* given a move and a piece returns if the move was valid *)
val is_valid_move: moves -> Piece.piece_type -> bool*)