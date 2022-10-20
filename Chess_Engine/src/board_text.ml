open Graphics
let rec write_letters lst size = 
  match lst with 
  |[] -> []
  |(x,y)::t when y - size = 0 -> Graphics.moveto (x+size/2) (y/2); Graphics.draw_char (Char.chr (64+x/size)); write_letters t size 
  |(x,y) :: t -> write_letters t size 