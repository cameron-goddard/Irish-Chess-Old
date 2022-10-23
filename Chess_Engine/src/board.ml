open Piece
open Tile


type t = {
  tile_list: Tile.tile list
}


let empty_tile_board_list  : t =  {tile_list = Tile.empty_tile_list 8 []}


let rec xy_generate (tiles : tile list) (acc : tile list) (x_inc : int) (y_inc : int) = 
  match tiles with 
  | [] -> acc 
  | h::t -> 
    if x_inc < 7 then 
         xy_generate t ((modify h x_inc  y_inc false Piece.empty_piece) :: acc) (x_inc + 1) (y_inc)
    else 
      if y_inc < 8 then xy_generate  t ((modify h x_inc y_inc false Piece.empty_piece) :: acc) (0) (y_inc+1)
      else acc
  

let rec init_pawn_position (tiles : tile list ) acc = 
  match tiles with 
    | [] -> acc
    | h :: t -> match Tile.get_coordinates h with 
                | (x,y) when y = 1 -> (modify h x y true (set_piece Piece.Pawn White)):: acc 
                | (x,y) when y = 6 -> (modify h x y true (set_piece Piece.Pawn Black)) :: acc 
                | _ -> init_pawn_position t (h:: acc) 


let get_tile_list (board: t) = 
  board.tile_list

let init_positioned_tiles = xy_generate (Tile.empty_tile_list 64 []) [] 0 0
let init_tiles =  init_pawn_position init_positioned_tiles []

let init (name: string) = {
  tile_list = init_tiles
}


