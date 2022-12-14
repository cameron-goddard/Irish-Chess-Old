type command =
  | Move of (int * int) * (int * int)
  | Castle of string
  | Load of string
  | Empty
  | Help
  | Info
  | Quit  (** The abstract type representing valid commands *)

exception InvalidCommand of string
(** [InvalidCommand] is raised when an invalid command is inputted by the user. *)

val parse : string -> command
(** [parse str] parses the inputted string [str] and returns a command. Raises
    InvalidCommand if the inputted string does not correspond to any valid
    command. *)
