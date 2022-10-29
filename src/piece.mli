(*type piece_type = Pawn | Knight | Bishop | Rook | Queen| King 
(** The abstract type representing a piece's type *)

type team
(** The abstract representing a piece's team *)

type pos
(** The abstract type representing a piece's position *)

type is_piece
(** The abstract record type representing a piece's positoin, team, and type *)

val get_piece_type: is_piece-> piece_type
(** [get_piece_type piece] is the piece_type that [piece] represents *)

val get_team: is_piece->team
(** [get_team piece] is the team that [piece] represents *)

val get_pos: is_piece -> pos
(** [get_team piece] is the position that [piece] represents *)

*)

type piece_type = Pawn | Knight | Bishop | Rook | Queen | King 
type color = White | Black

type piece

val empty_piece : piece
val set_piece : piece_type -> color -> piece
val get_piece_type : piece -> piece_type 
val get_piece_color : piece -> color 
