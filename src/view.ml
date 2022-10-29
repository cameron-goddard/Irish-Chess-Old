open Graphics
open Tile
open Board
open Piece

let print_tile_coordinates tile = 
  print_int (Tile.get_x tile); print_int (Tile.get_y tile)

let rec print_tile_list (tiles : tile list ) = 
  match tiles with 
  |[] -> print_newline
  |h ::t -> print_tile_coordinates h; print_tile_list t

let rec make_positions_x num_pos lst x y inc = 
  if List.length lst = num_pos then lst
  else make_positions_x num_pos ((x+inc,y)::lst) (x+inc) y inc

let rec make_positions_y num_pos lst x y inc = 
  if List.length lst = num_pos then lst
  else make_positions_y num_pos ((x,y+inc)::lst) x (y+inc) inc

let rec make_combined_positions pos x_lst inc acc = 
  match x_lst with 
    | [] -> acc
    | (x, y) :: t -> acc @ make_positions_y pos [] x y inc @ make_combined_positions pos t inc acc


let colored_tile clr left_x left_y size = Graphics.set_color clr; Graphics.fill_rect left_x left_y size size

let rec draw_piece (tiles : tile list)  = 
  match tiles with
    | [] -> 
      let _ = "empty" in
      []
    | h::t -> 
    if Tile.has_piece h then 
      (* let _ = print_tile_coordinates h in *)
      if Piece.get_piece_color (Tile.get_piece h) = White then 
        let _ = colored_tile Graphics.white ((get_x h + 1)*50 + 5) ((get_y h + 1)*50 + 5) 40 in 
        draw_piece t 
      else 
         let _ = colored_tile Graphics.magenta ((get_x h + 1)*50 + 5) ((get_y h + 1)*50 + 5) 40 in 
        draw_piece t 
    else 
      (* let _ = print_tile_coordinates h in *)
      (* let _ = colored_tile Graphics.white ((get_x h + 1)*50 + 5) ((get_y h + 1)*50 + 5) 40 in *)
      draw_piece t
      
      (*if Tile.has_piece h then 
      (colored_tile Graphics.white ((get_x h)*size) ((get_y h)*size) size); 
      (draw_piece t size) else 
        draw_piece t size*)


let size = 50
(*List of x coords*)
let x_lst = make_positions_x 8 [] 0 0 size

(*List of x and y coords*)
let xy_lst = make_combined_positions 8 x_lst size []


let init = 
  Graphics.open_graph "";
  set_window_title "Irish Chess"; ()


let rec draw_helper xy_lst =
  match xy_lst with 
    | [] -> []
    | (x,y) :: t when Int.abs (x-y) mod 20 = 0 -> 
      colored_tile Graphics.black x y size ;
      draw_helper t
    | (x,y) :: t ->
      colored_tile Graphics.cyan x y size; 
      draw_helper t

let draw_board board = 
  draw_helper xy_lst;
  print_tile_list (Board.get_tile_list (Board.init "woah"));
  draw_piece (Board.get_tile_list (Board.init "woah")) ;
  
  ()
