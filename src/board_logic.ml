(** shows the position on the board with a corresponding square and pixel on the bottom left corner*)
type position_on_board = {square_number : (char * int); pixel_number : (int * int)}


(*takes in a list of points in pixel values and returns a list of records that pairs each point with a coordanate of each piece(a letter and a number)*)
let rec generate_val_coordinates lst_of_pts size acc= 
  match lst_of_pts with 
  |[] -> acc
  |(x,y)::t -> generate_val_coordinates t size ({square_number = (Char.chr (64+x/size), y/size); pixel_number = (x,y)} :: acc); 



