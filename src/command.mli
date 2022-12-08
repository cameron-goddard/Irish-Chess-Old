type command =
  | Move of (int * int) * (int * int)
  | Castle of string
  | Empty
  | Help
  | Info
  | Quit

exception InvalidCommand of string

val parse : string -> command