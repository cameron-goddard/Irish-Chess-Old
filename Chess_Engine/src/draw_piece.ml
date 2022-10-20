open Graphics

let graph_pawn clr = Graphics.set_color clr; Graphics.fill_poly [|(50,100);(100,100);(75,150)|]
let graph_pawn clr = Graphics.set_color clr; Graphics.fill_poly [|(100,100);(150,100);(125,150)|]



let rec draw_close_row_of_pawns init_size curr pwn_nbr  = 
match pwn_nbr with
  |pwn_nbr when pwn_nbr < 8-> Graphics.fill_poly [|(curr,2*init_size);(curr+init_size,2*init_size);(curr+init_size/2,3*init_size)|]; draw_close_row_of_pawns init_size (curr+init_size) (pwn_nbr + 1)
  |_ -> ()
   