open Piece
open Tile

type t
(** The abstract type of values representing the game board. *)

val get_tile_list: t -> Tile.tile list
(** [get_tile_list t] is the list of tiles from which the game board represents. *)

val init: string -> t
