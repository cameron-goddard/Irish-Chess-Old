open Graphics
open Board
open Piece
open Util
open Command
open Yojson.Basic.Util

type display_colors = Graphics.color * Graphics.color

type t = {
  name : string;
  colors : display_colors;
  mutable turn : string;
  board : Board.t;
}

type color =
  | Light
  | Dark

type piece_container = {
  king : piece list;
  queen : piece list;
  bishops : piece list;
  knights : piece list;
  rooks : piece list;
  pawns : piece list;
}

let get_board t : Board.t = t.board

let help_message =
  "\n\
   HELP:\n\n\
  \    Move [start] [end]: Moves the piece at [start] to the tile [end]. An \
   error message will be displayed if \n\
  \   there is no piece at [start], or if the move is invalid. \n\n\
  \   \n\
  \    Load [file]: Loads the JSON file [file] from the data directory. An \
   error message will be displayed if \n\
  \ the JSON file to load is in an invalid format, or if no such file exists. \n\
  \    \n\
  \    Help: Displays this helpful message.\n\n\
  \    Info: Displays some info about this program.\n\
  \    \n\
  \    Quit: Quits the program.\n\
  \    "

let info_message =
  "Irish Chess created by Anthony Pizzolato, Cameron Goddard, Chris Price, and \
   Sahib Manjal"

let display_colors_of_json j =
  let l = to_list j in
  let light_color = hex_to_rgb (to_string (List.nth l 0)) in
  let dark_color = hex_to_rgb (to_string (List.nth l 1)) in

  ( rgb (List.nth light_color 0) (List.nth light_color 1)
      (List.nth light_color 2),
    rgb (List.nth dark_color 0) (List.nth dark_color 1) (List.nth dark_color 2)
  )

let piece_location_of_json m j =
  let mem = j |> member m |> to_string in
  coords_of_notation mem

let piece_list_of_json c pt j =
  let pieces = j |> member (string_of_piece_type pt) |> to_list in
  let rec ret p =
    match p with
    | [] -> []
    | h :: t ->
        let str = to_string h in
        (* Printf.printf "%s %s at %s\n" (Piece.string_of_color c)
           (Piece.string_of_piece_type pt) str; *)
        Piece.create pt c
          (fst (coords_of_notation str))
          (snd (coords_of_notation str))
        :: ret t
  in
  ret pieces

let piece_container_of_json c j =
  {
    king = piece_list_of_json c King j;
    queen = piece_list_of_json c Queen j;
    bishops = piece_list_of_json c Bishop j;
    knights = piece_list_of_json c Knight j;
    rooks = piece_list_of_json c Rook j;
    pawns = piece_list_of_json c Pawn j;
  }

let piece_list_of_container container =
  match container with
  | { king; queen; bishops; knights; rooks; pawns } ->
      king @ queen @ bishops @ knights @ rooks @ pawns

let t_of_json j =
  {
    name = j |> member "name" |> to_string;
    colors = j |> member "colors" |> display_colors_of_json;
    turn = "White";
    board =
      (j |> member "light_init"
      |> piece_container_of_json White
      |> piece_list_of_container)
      @ (j |> member "dark_init"
        |> piece_container_of_json Black
        |> piece_list_of_container);
  }

let from_json json =
  try t_of_json json with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let rec game_step mode t (b : Board.t) =
  if mode = "cli" then (
    print_endline (View.print_board b t.name t.turn);
    print_string "> ";
    try
      let input = parse (read_line ()) in
      ANSITerminal.erase Screen;
      match input with
      | Move (st, en) -> process_move mode t b st en
      | Castle dir ->
          print_endline "Sike you thought";
          game_step mode t b
      | Load file -> (
          try
            let f = Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ file) in
            let new_game = from_json f in
            print_endline
              ("\nLoading in file \"" ^ "data" ^ Filename.dir_sep ^ file ^ "\"");
            game_step mode new_game new_game.board
          with Command.InvalidCommand str ->
            print_endline ("Error: " ^ str);
            game_step mode t b)
      | Help ->
          print_endline help_message;
          game_step mode t b
      | Info ->
          print_endline info_message;
          game_step mode t b
      | Empty -> game_step mode t b
      | Quit -> exit 0
    with Command.InvalidCommand str ->
      ANSITerminal.erase Screen;
      print_endline "Invalid command";
      game_step mode t b)
  else if mode = "gui/w_text" then (
    View.draw_board b;
    print_string "> ";
    try
      let input = parse (read_line ()) in
      match input with
      | Move (st, en) -> process_move mode t b st en
      | Castle dir -> raise (Failure "Unimplemented")
      | Help -> raise (Failure "Unimplemented")
      | Info ->
          Printf.printf "pressed info\n";
          game_step mode t b
      | Empty -> game_step mode t b
      | Quit -> exit 0
      | _ -> game_step mode t b
    with _ ->
      print_endline "INVALID MOVE ... try again";
      game_step mode t b)
  else if mode = "gui/no_text" then (
    try
      View.draw_board b;
      let start_status = Graphics.wait_next_event [ Button_down ] in
      let st =
        ((start_status.mouse_x / 50) - 1, (start_status.mouse_y / 50) - 1)
      in
      let end_status = Graphics.wait_next_event [ Button_down ] in
      let en = ((end_status.mouse_x / 50) - 1, (end_status.mouse_y / 50) - 1) in
      process_move mode t b st en;
      print_endline
        (string_of_int start_status.mouse_x ^ string_of_int start_status.mouse_y);
      game_step mode t b
    with _ ->
      print_endline "CLICK ON THE BOARD";
      game_step mode t b)
  else
    let event = Graphics.wait_next_event [ Key_pressed ] in
    if event.key == 'q' then exit 0
    else if event.button then game_step mode t b
    else game_step mode t b

and process_move mode t b st en =
  try
    match Board.piece_at st b with
    | Some p ->
        if Piece.string_of_color (Piece.get_piece_color p) <> t.turn then ()
        else if t.turn = "white" then t.turn <- "black"
        else t.turn <- "white";
        let b' = Board.update b st en in
        game_step mode t b'
    | None -> print_endline "no"
  with Board.InvalidMove s -> print_endline s

let main () =
  let mode = Sys.argv.(1) in
  let arg = Sys.argv.(2) in
  let f = Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ arg) in
  let t = from_json f in
  if mode = "cli" then (
    ANSITerminal.erase Screen;
    ANSITerminal.print_string [ ANSITerminal.green ]
      "☘ Welcome to Irish Chess ☘\n";
    game_step mode t t.board)
  else
    let _ = View.init in
    game_step mode t t.board
