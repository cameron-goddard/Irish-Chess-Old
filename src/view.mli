val init : unit

val print_board : Board.Board.t -> string -> string -> string
(** [print_board b] is responsible for printing the board [b] to the screen. *)

val draw_board : Board.Board.t -> unit
(** [draw_board b] is responsible for drawing the board [b] to the screen. *)
