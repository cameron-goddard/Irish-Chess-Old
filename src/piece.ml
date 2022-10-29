type piece_type = Pawn | Knight | Bishop | Rook | Queen | King 

type color = White | Black

type piece = {
  piece_type : piece_type;
  color : color;
}

let empty_piece = {
  piece_type = Pawn;
  color = White;
}

let set_piece (piece:piece_type) (color_of_piece: color) : piece = {piece_type = piece; color = color_of_piece}

let get_piece_type (piece: piece) = piece.piece_type

let get_piece_color (piece:piece) = piece.color