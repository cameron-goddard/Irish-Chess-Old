open Graphics

let rec game_step (board: Board.t) =
  Graphics.clear_graph ();
  View.draw_board board;
  let event = Graphics.wait_next_event [Key_pressed] in
  if event.key == 'q' then exit 0 else
  if event.button then
    game_step board
  else 
    game_step board

let main () =
  let _ = View.init in
    let board = Board.init "" in
      game_step board