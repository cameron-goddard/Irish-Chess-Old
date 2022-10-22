open Graphics
open Piece

type tile

val modify : tile -> int -> int -> bool -> piece -> tile
val empty_tile : tile
val empty_tile_list : int -> tile list -> tile list
val get_coordinates : tile -> (int * int) 
val has_piece : tile -> bool
val get_x : tile -> int
val get_y : tile -> int 
