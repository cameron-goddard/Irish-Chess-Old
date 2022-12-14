(** [piece_type] is the type of chess piece that each piece could be i.e.
    (pawn,knight,bishop,rook,queen,king). *)
type piece_type =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

(** [color] represents the color of the piece indicating which team it is on,
    White or Black. *)
type color =
  | White
  | Black

type piece = {
  piece_type : piece_type;
  color : color;
  x : int;
  y : int;
}
(** piece represent a piece on the chess Board it has a piece_type of type
    piece_type a color of type color and integers x y representing (x,y)
    coordinate of the piece on the board. *)

val string_of_piece_type : piece_type -> string
(** [string_of_piece_type piece_type] converts a given piece_type to a string *)

val string_of_color : color -> string
(** [string_of_color color] converts the type [color] to a string. *)

val empty_piece : piece
(** [empty_piece] returns a white pawn at (0,0). *)

val set_piece : piece_type -> color -> piece
(** [set_piece piece color] creates a piece at (0,0) of type piece and color
    [color]. *)

val get_piece_type : piece -> piece_type
(** [get_piece_type piece] returns the piece type of a given piece. *)

val get_piece_color : piece -> color
(** [get_piece_color piece] returns the color of the given piece [piece]. *)

val piece_loc : piece -> int * int
(** [piece_loc piece] returns the (x,y) coordinats of a given piece. *)

val create : piece_type -> color -> int -> int -> piece
(** [create t c x y] creates a piece with type [t] color [c] and coordinates
    [x],[y]. *)
