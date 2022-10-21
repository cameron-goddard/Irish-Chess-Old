type piece_type = Pawn | Knight | Bishop | Rook | Queen| King 

type team

type pos

type is_piece

val get_piece_type: is_piece-> piece_type

val get_team: is_piece->team

val get_pos: is_piece -> pos