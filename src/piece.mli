
type piece
(** The abstract type of values representing chess pieces. *)

type piece_type = Pawn | Knight | Bishop | Rook | Queen | King 
(** The type representing the different types of standard chess pieces. *)

type color = White | Black
(** The type representing the color of a piece. *)

val empty_piece : piece
val set_piece : piece_type -> color -> piece
val get_piece_type : piece -> piece_type 
val get_piece_color : piece -> color 
