open Chess.Tile
open Chess.Draw_piece
open Graphics
(* let _ = open_graph ""
let _ = set_color blue
let _ = fill_rect 0 0 100 100 *)




let () =
  view_board;
  Graphics.set_color white;
  draw_close_row_of_pawns (size_of_square) (size_of_square) 0;
  Graphics.set_color black;
  draw_far_row_of_pawns (size_of_square) (size_of_square) 0;
  Unix.sleep 10;
  print_string "Hi";
