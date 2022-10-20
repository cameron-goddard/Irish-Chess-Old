open Graphics
(* 
type position =  char * int 

type tile = { psn : position; square_color : color }


let black_tile  = { psn = ('A',5) ;  square_color = cyan} *)

let colored_tile clr left_x left_y heigth width = Graphics.set_color clr; Graphics.fill_rect left_x left_y heigth width

type board_position = { list_of_points: (int * int) list; size_of_square:int; 
                        bottom_left : (int * int); bottom_right: (int * int); 
                        top_left: (int * int); top_right : (int * int)}
let positions = []



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

let rec quick_graph lst heigth width = 
  match lst with 
    | [] -> []
    | (x,y)::t when (x + y) mod (heigth+width) = 0 -> colored_tile Graphics.red x y heigth width; quick_graph t heigth width
    | (x,y) :: t -> colored_tile Graphics.cyan x y heigth width; quick_graph t heigth width
let size = 50

let x_lst = make_positions_x 8 [] 0 0 size
let z_lst = make_combined_positions 8 x_lst size []


let graphed_board_position = {list_of_points = z_lst; size_of_square = size; 
                      bottom_left = (size,size); bottom_right = (size, size*8); 
                      top_left = (size,size*8); top_right = (size*8,size*8)}
let four_corners = graphed_board_position.bottom_right :: graphed_board_position.bottom_left :: graphed_board_position.top_right ::graphed_board_position.top_left :: []
let view_board = Graphics.open_graph ""; 
                 quick_graph (graphed_board_position.list_of_points) size size;




(* 
let rec draw_one_row lst  = 
  match lst with 
    | [] -> []
    | (x ,y)::t -> colored_tile Graphics.black x y 20 20; draw_one_row t
    | (x ,y)::t -> colored_tile Graphics.cyan x y 20 20; draw_one_row t
   *)

(* let rec testing_pattern_match lst = 
  match lst with 
  |[] -> []
  |(x , y)::t when x mod 40 = 5 -> x; testing_pattern_match t

 *)



(* let rec draw_one_row lst  = 
  match lst with 
    | [] -> []
    | (x,y)::t -> if x mod 40 = 0  then 
      let _ = colored_tile Graphics.black x y 
        in draw_one_row t 
  else 
    let _ = colored_tile Graphics.cyan x y 
        in draw_one_row t
  
(*  *)
let display_one_row = make_positions_x [] 0 0 |> draw_one_row
let display_one_y_row = make_positions_y 8 [] 0 0 40 |> draw_one_row

let rec display_eight num_rows row_0 y_inc init_y_inc = 
  if row_0 = num_rows then make_positions_x num_rows [] 0 160 init_y_inc |> draw_one_row
  else display_eight (row_0+1) (y_inc+init_y_inc) init_y_inc; make_positions_x num_rows [] 0 (y_inc) init_y_inc |> draw_one_row;
 *)



(* Generate positions x's spaced out by W's, and y's spaced out by H's 
For example takes input (rows, 8), (cols, 8) and spits out coordinates for the edges of each square *)


(* let _ = open_graph ""
let _ = draw_rect 0 0 5 5;  *)


