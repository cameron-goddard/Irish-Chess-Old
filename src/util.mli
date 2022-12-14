val hex_to_rgb : string -> int list
(** [hex_to_rgb hex] is the converted r, g, b values of the hex color passed in.
    Requires: [hex] is a valid hexadecimal color. *)

val coords_of_notation : string -> int * int
(** [coords_of_notation str] is the row/col equivalent to the chess-notation
    string [str]. Requires: [str] is a valid chess-notation location. *)

val notation_of_coords : int -> int -> string
(** [notation_of_coords r c] is the chess-notation equivalent to the row [r] and
    column [c] of the internal board system. Requires: [r] and [c] are valid
    rows and columns for a standard chess board. *)
