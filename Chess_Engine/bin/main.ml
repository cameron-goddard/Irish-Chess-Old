open Chess.Tile
open Chess.Draw_piece
open Graphics
(* let _ = open_graph ""
let _ = set_color blue
let _ = fill_rect 0 0 100 100 *)


let generate_starting_left_square pos size num = 
  match pos with 
    |(x,y) -> (x+size*(num-1), y)

let generate_starting_right_square pos size num= 
  match pos with 
    |(x,y) -> (x-size*(num-1), y)



let () =
  view_board;
  Graphics.set_color white;
  draw_close_row_of_pawns (size_of_square) (size_of_square) 0;
  draw_rook size_of_square bottom_left;
  draw_rook size_of_square bottom_right;
  draw_knight size_of_square (generate_starting_left_square bottom_left size_of_square 2) ;
  draw_knight size_of_square (generate_starting_right_square bottom_right size_of_square 2) ;
  draw_bishop size_of_square (generate_starting_right_square bottom_right size_of_square 3);
  draw_bishop size_of_square (generate_starting_left_square bottom_left size_of_square 3);
  draw_king size_of_square (generate_starting_left_square bottom_left size_of_square 5);
  draw_queen size_of_square (generate_starting_left_square bottom_left size_of_square 4);
  Graphics.set_color black;
  draw_far_row_of_pawns (size_of_square) (size_of_square) 0;
  draw_rook size_of_square top_left;
  draw_rook size_of_square top_right;
  draw_knight size_of_square (generate_starting_left_square top_left size_of_square 2 ) ;
  draw_knight size_of_square (generate_starting_right_square top_right size_of_square 2) ;
  draw_bishop size_of_square (generate_starting_right_square top_right size_of_square 3);
  draw_bishop size_of_square (generate_starting_left_square top_left size_of_square 3);
  draw_king size_of_square (generate_starting_left_square top_left size_of_square 5);
  draw_queen size_of_square (generate_starting_left_square top_left size_of_square 4);
  Unix.sleep 100;
  print_string "Hi";
