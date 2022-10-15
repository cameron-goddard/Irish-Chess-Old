open Chess
(* open Graphics *)

(* let _ = open_graph ""
let _ = set_color blue
let _ = fill_rect 0 0 100 100 *)



let () =
  Graphics.open_graph "";
  Graphics.fill_rect 0 0 100 100;
  Unix.sleep 10;
  print_string "Hi";
