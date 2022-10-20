open Chess.Tile
open Chess.Draw_piece
open Graphics
(* let _ = open_graph ""
let _ = set_color blue
let _ = fill_rect 0 0 100 100 *)




let () =
  view_board;
  Graphics.set_color white;
  draw_close_row_of_pawns 50 50 0;
  Unix.sleep 10;
  print_string "Hi";
