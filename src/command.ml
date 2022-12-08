open Util

type command =
  | Move of (int * int) * (int * int)
  | Castle of string
  | Empty
  | Help
  | Info
  | Quit

exception InvalidCommand of string

let parse input =
  let trimmed = String.trim input in
  let items = String.split_on_char ' ' trimmed in
  let items_trimmed = List.filter (fun x -> x <> "") items in
  if List.length items_trimmed = 0 then Empty
  else
    match List.nth items_trimmed 0 with
    | "move" ->
        Move
          ( coords_of_notation (List.nth items_trimmed 1),
            coords_of_notation (List.nth items_trimmed 2) )
    | "castle" -> Castle ""
    | "help" -> Help
    | "info" -> Info
    | "quit" -> Quit
    | _ -> raise (InvalidCommand "Unknown command")
