type piece_type =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

type color =
  | White
  | Black

type piece = {
  piece_type : piece_type;
  color : color;
  x : int;
  y : int;
}

let string_of_piece_type pt =
  match pt with
  | King -> "king"
  | Queen -> "queen"
  | Bishop -> "bishop"
  | Knight -> "knight"
  | Rook -> "rook"
  | Pawn -> "pawn"

let string_of_color c =
  match c with
  | White -> "white"
  | Black -> "black"

let create t c x y = { piece_type = t; color = c; x; y }
let empty_piece = { piece_type = Pawn; color = White; x = 0; y = 0 }

let set_piece (piece : piece_type) (color_of_piece : color) : piece =
  { piece_type = piece; color = color_of_piece; x = 0; y = 0 }

let get_piece_type (piece : piece) = piece.piece_type
let get_piece_color (piece : piece) = piece.color
let piece_loc piece = (piece.x, piece.y)
