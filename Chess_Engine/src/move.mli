type moves

(*type conditional_move*)

type move_list

val moves_for_piece: Piece.piece_type -> move_list

val is_valid_move: moves -> Piece.piece_type -> bool