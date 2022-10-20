open Graphics
let rec write_letters lst size = 
  match lst with 
  |[] -> []
  |(x,y)::t when y - size = 0 -> Graphics.moveto (x+size/2) (y/2); Graphics.draw_char (Char.chr (64+x/size)); write_letters t size 
  |(x,y) :: t -> write_letters t size 


let rec write_numbers lst size = 
  match lst with 
  |[] -> []
  |(x,y)::t when x - size = 0 -> Graphics.moveto (x/2) (y+size/2); Graphics.draw_string (string_of_int (y/size)); write_numbers t size 
  |(x,y) :: t -> write_numbers t size 