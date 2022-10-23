open Graphics
open Piece
let graph_pawn clr = Graphics.set_color clr; Graphics.fill_poly [|(50,100);(100,100);(75,150)|]
let graph_pawn clr = Graphics.set_color clr; Graphics.fill_poly [|(100,100);(150,100);(125,150)|]



let rec draw_close_row_of_pawns init_size curr pwn_nbr  = 
match pwn_nbr with
  |pwn_nbr when pwn_nbr < 8-> Graphics.fill_poly [|(curr,2*init_size);(curr+init_size,2*init_size);(curr+init_size/2,3*init_size)|]; draw_close_row_of_pawns init_size (curr+init_size) (pwn_nbr + 1)
  |_ -> ()

let rec draw_far_row_of_pawns init_size curr pwn_nbr  = 
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
  | (x,y) -> Graphics.fill_poly [|(size*x+size/3,y);(size*x+2*size/3,y);(size*x,y+size/2);(size*x+size/2,size*y+size);(size*x+size,size*y+size/2)|]


let draw (piece: Piece.piece) size pos  = 
  match get_piece_type piece with 
    |Pawn -> ()
    |Queen -> draw_queen size pos
    |King -> draw_king size pos
    |Bishop -> draw_bishop size pos
    |Knight -> draw_knight size pos
    |Rook -> draw_rook size pos





