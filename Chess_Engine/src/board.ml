open Piece
open Tile

type t = {
  tile_list: Tile.tile list
}


let get_tile_list (board: t) = 
  board.tile_list


let rec init_lst lst = 
  if List.length lst = 8 then lst else
  init_lst (Tile.empty_tile :: lst)

let rec init_tiles = init_lst []


let init (name: string) = {
  tile_list = init_tiles
}


