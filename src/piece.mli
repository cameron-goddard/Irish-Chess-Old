type piece_type =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
      (** The type representing the different types of standard chess pieces. *)

type color =
  | White
  | Black  (** The type representing the color of a piece. *)

type piece = {
  piece_type : piece_type;
  color : color;
  x : int;
  y : int;
}
(** The abstract type of values representing chess pieces. *)

val string_of_piece_type : piece_type -> string
val string_of_color : color -> string
val empty_piece : piece
val set_piece : piece_type -> color -> piece
val get_piece_type : piece -> piece_type
val get_piece_color : piece -> color
val piece_loc : piece -> int * int
val create : piece_type -> color -> int -> int -> piece