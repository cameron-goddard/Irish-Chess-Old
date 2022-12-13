open Piece

(** A [Board] is a standard chess board. *)
module Board : sig
  type t = piece list
  (** [t] is the type of a board, represented as the pieces on the board *)

  val piece_at : int * int -> t -> piece option
  (* [piece_at (x, y) b] is [Some p] if there is a piece at [(x, y)], otherwise
     it is [None]. *)

  val update : t -> int * int -> int * int -> t
  (** [update b (xi, yi) (xf, yf)] is the updated board after moving the piece
      at [(xi, yi)] to [(xf, yf)]. Raises [InvalidMove s] if the move cannot be
      completed. Requires: (xi, yi) is a valid piece to move. *)

  val graphics_rep : t -> (int * int) list
  (** [graphics_rep b] is the necessary information required to display [b] in a
      GUI or CLI. *)

  exception InvalidMove of string
  (** [InvalidMove] is raised when a piece move cannot be completed. *)
  exception Checkmate of string
end