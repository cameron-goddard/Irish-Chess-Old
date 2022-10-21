open Graphics
open Tile



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

let size = 50

(*List of x coords*)
let x_lst = make_positions_x 8 [] 0 0 size

(*List of x and y coords*)
let xy_lst = make_combined_positions 8 x_lst size []


let init = 
  Graphics.open_graph "";
  set_window_title "Chess"; ()

let colored_tile clr left_x left_y heigth width = Graphics.set_color clr; Graphics.fill_rect left_x left_y heigth width

let rec draw_helper xy_lst =
  match xy_lst with 
    | [] -> []
    | (x,y) :: t when Int.abs (x-y) mod 20 = 0 -> 
      print_string "~~~~";
      print_int x;
      print_string ", ";
      print_int y;
      print_string "~~~~\n";
      colored_tile Graphics.black x y size size;
      draw_helper t
    | (x,y) :: t ->
      print_string "~~~~";
      print_int x;
      print_string ", ";
      print_int y;
      print_string "~~~~\n";
      colored_tile Graphics.cyan x y size size; 
      draw_helper t

let draw_board board = 
  draw_helper xy_lst; ()

(*let draw_pieces =*)



  





(*let rec draw_close_row_of_pawns init_size curr pwn_nbr = 
  match pwn_nbr with
    |pwn_nbr when pwn_nbr < 8-> Graphics.fill_poly [|(curr,2*init_size);(curr+init_size,2*init_size);(curr+init_size/2,3*init_size)|]; draw_close_row_of_pawns init_size (curr+init_size) (pwn_nbr + 1)
    |_ -> ()

let rec draw_far_row_of_pawns init_size curr pwn_nbr = 
  match pwn_nbr with
    |pwn_nbr when pwn_nbr < 8-> Graphics.fill_poly [|(curr,7*init_size);(curr+init_size,7*init_size);(curr+init_size/2,8*init_size)|]; draw_far_row_of_pawns init_size (curr+init_size) (pwn_nbr + 1)
    |_ -> ()
     
let draw_rook size pos = 
  match pos with 
    | (x,y) -> Graphics.fill_rect (x+size/5) (y+size/20) (3*size/5) (size*9/10)
  
let draw_knight size pos = 
  match pos with 
    | (x,y) -> Graphics.fill_ellipse (x+(size/2)) (y+(size/2)) (size/4) (size/2)
  
let draw_bishop size pos = 
  match pos with 
    | (x,y) -> Graphics.fill_poly [|(x+size/2,y);(x,y+size/2);(x+size/2,y+size);(x+size,y+size/2)|]
  
let draw_king size pos = 
  match pos with 
    | (x,y) -> Graphics.fill_circle (x+size/2) (y+size/2) (size/2)

  
let draw_queen size pos = 
  match pos with 
  | (x,y) -> Graphics.fill_poly [|(x+size/3,y);(x+2*size/3,y);(x,y+size/2);(x+size/2,y+size);(x+size,y+size/2)|]*)

  
  
  
  