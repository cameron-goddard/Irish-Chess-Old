
type position_on_board = {square_number : (char * int); pixel_number : (int * int)}


let rec generate_val_coordinates lst_of_pts size acc= 
  match lst_of_pts with 
  |[] -> acc
  |(x,y)::t -> generate_val_coordinates t size ({square_number = (Char.chr (64+x/size), y/size); pixel_number = (x,y)} :: acc); 



