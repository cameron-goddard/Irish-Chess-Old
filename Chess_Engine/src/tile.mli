(* 

type position 
(** The abstract type representing position on board *)

type tile 
(** The abstract type representing a tile of the chess board *)
 *)
 type board_position

 val view_board : 'a list
val four_corners : (int * int) list
val size_of_square : int
val top_left : (int * int)
val top_right : (int * int)
val bottom_left : (int * int)
val bottom_right : (int * int)
val coordinates : (int * int) list


val graphed_board_position : board_position
